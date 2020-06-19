{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-|
Module      : Knit.Effect.AtomicCache
Description : Effect for managing a persistent cache of serializable things to avoid redoing computations
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module implementes the `DataCache` polysemy effect for managing a keyed cache of serialized
things.  It allows simple maintaining of disk-backed storage of computational results as long as
they are serializable.

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.
-}
module Knit.Effect.AtomicCache
  (
    -- * Effect
    Cache
    -- * TimeStamps
  , WithCacheTime
  , ActionWithCacheTime
  , wctMapAction
  , unWithCacheTime
  , cacheTimeM
  , getCachedAction
--  , sequenceCacheTimesM
    -- * Actions
  , encodeAndStore
  , retrieveAndDecode
  , lookupAndDecode
  , retrieveOrMake
  , clear
  , clearIfPresent
    -- * Serialization
  , Serialize(..)
    -- * Persistance
  , persistAsByteString
  , persistAsStrictByteString
  , persistAsByteStreamly
  , persistAsByteArray
    -- * Interpretations
  , runAtomicInMemoryCache
  , runBackedAtomicInMemoryCache
  , runPersistenceBackedAtomicInMemoryCache
  , runPersistenceBackedAtomicInMemoryCache'
    -- * Exceptions
  , CacheError(..)
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import qualified Knit.Effect.Logger            as K

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
--import qualified Data.Foldable as           Fold
import           Data.Functor.Identity          (Identity(..))
import           Data.Int (Int64)
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import qualified Data.Word                     as Word

import qualified Control.Concurrent.STM        as C
import qualified Control.Exception             as Exception
import           Control.Monad                  ( join )

import qualified Streamly                      as Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Internal.Memory.Array         as Streamly.Array
import qualified Streamly.FileSystem.Handle    as Streamly.Handle
import qualified Streamly.Internal.FileSystem.File as Streamly.File

import qualified System.IO                     as System
import qualified System.Directory              as System
import qualified System.IO.Error               as IO.Error


{- TODO:
1. Can this deisgn be simplified, part 1. The Maybe in the TMVar seems like it should be uneccessary.
2. It'd be nice to make sure we can't leave the empty TVar. Can this be done in a way so that it must be filled?
3. We should be able to factor out some things around handling the returned TMVar
-}
-- | Error Type for Cache errors.  Simplifies catching them and reporting them.
data CacheError =
  ItemNotFoundError T.Text
  | ItemTooOldError T.Text
  | DeSerializationError T.Text
  | PersistError T.Text
  | OtherCacheError T.Text deriving (Show, Eq)

-- | Type to carry encoding/decoding functions for Serializing data.  Allows for different Serializers as well as
-- Serializing to different types of in memory store.
-- @encode@ returns the encoded value *and* a (possibly buffered) copy of its input in the case where the imput is expensive to produce.
-- This is designed around serialization of streams, where the original stream may be expensive to run but once run,
-- we can return a "buffered" stream which just unfolds from a memory buffer.
-- In many cases, we will just return the input in that slot.
data Serialize r a ct where
  Serialize :: (P.MemberWithError (P.Error CacheError) r)
            => (a -> P.Sem r (ct, a)) -- encode
            -> (ct -> P.Sem r a) -- decode
            -> (ct -> Int64) -- size (in Bytes)
            -> Serialize r a ct

-- | Wrapper to hold content and a timestamp
-- The stamp must be at or after the time the data was constructed
data WithCacheTime m a where
  WithCacheTime :: Maybe Time.UTCTime -> m a -> WithCacheTime m a
  deriving (Show)

instance Functor m => Functor (WithCacheTime m) where
  fmap f (WithCacheTime tM ma) = WithCacheTime tM (fmap f ma)

instance Applicative m => Applicative (WithCacheTime m) where
  pure x = WithCacheTime Nothing (pure x)
  WithCacheTime t1M mf <*> WithCacheTime t2M ma = WithCacheTime (maxMaybeTime t1M t2M) (mf <*> ma)

maxMaybeTime :: Maybe Time.UTCTime -> Maybe Time.UTCTime -> Maybe Time.UTCTime
maxMaybeTime Nothing t2M = t2M
maxMaybeTime t1M Nothing = t1M
maxMaybeTime (Just t1) (Just t2) = Just $ max t1 t2

onlyCacheTime :: Applicative m => Maybe Time.UTCTime -> WithCacheTime m ()
onlyCacheTime tM = WithCacheTime tM (pure ())

wctApplyNat :: (forall a. f a -> g a) -> WithCacheTime f b -> WithCacheTime g b
wctApplyNat nat (WithCacheTime tM fb) = WithCacheTime tM (nat fb)

wctMapAction :: (m a -> n b) -> WithCacheTime m a -> WithCacheTime n b
wctMapAction f (WithCacheTime tM ma) = WithCacheTime tM (f ma)

toSem :: Identity a -> P.Sem r a
toSem = pure . runIdentity

-- NB, this allows merging dependencies for passing to things which need them
-- as in:
-- let cachedDeps = (,,) <$> cached1 <*> cached2 <*> cached3

unWithCacheTime :: WithCacheTime m a -> m a
unWithCacheTime (WithCacheTime _ ma) = ma

cacheTimeM :: WithCacheTime m a -> Maybe Time.UTCTime
cacheTimeM (WithCacheTime tM _) = tM

type ActionWithCacheTime r a = WithCacheTime (P.Sem r) a
getCachedAction :: K.Sem r (ActionWithCacheTime r a) -> P.Sem r a
getCachedAction = (>>= unWithCacheTime)

--fmapActionWithCacheTime :: (a -> b) -> ActionWithCacheTime r a -> ActionWithCacheTime r b
--fmapActionWithCacheTime f = fmap (fmap f)

{-
sequenceCacheTimesM :: (Functor f, Foldable f) => f (WithCacheTime a) -> Maybe (WithCacheTime (f a))
sequenceCacheTimesM cts =
  let latestM = Fold.foldl' (\lt wc -> max (Just $ cacheTime wc) lt) Nothing cts
  in case latestM of
    Nothing -> Nothing
    Just latest -> Just $ WithCacheTime latest (fmap unWithCacheTime cts)
-}
-- | This is a Key/Value store
-- | Tagged by @t@ so we can have more than one for the same k and v
-- | h is a type we use for holding a resource
data Cache k v m a where
  CacheLookup :: k -> Cache k v m (Maybe (WithCacheTime Identity v))
  CacheUpdate :: k -> Maybe v -> Cache k v m () -- NB: this requires some way to attach a cache time during update
  
P.makeSem ''Cache

-- | Combinator to combine the action of serializing and caching
encodeAndStore
  :: forall ct k a r.
     ( Show k
     , P.Member (Cache k ct) r
     , K.LogWithPrefixesLE r
     )
  => Serialize r a ct
  -> k
  -> a
  -> P.Sem r ()
encodeAndStore (Serialize encode _ encBytes) k x =
  K.wrapPrefix ("AtomicCache.encodeAndStore (key=" <> (T.pack $ show k) <> ")") $ do
    K.logLE K.Diagnostic $ "encoding (serializing) data for key=" <> (T.pack $ show k) 
    encoded <- fst <$> encode x
    let nBytes = encBytes encoded
    K.logLE K.Diagnostic $ "Storing " <> (T.pack $ show nBytes) <> " bytes of encoded data in cache for key=" <> (T.pack $ show k) 
    cacheUpdate k (Just encoded)
{-# INLINEABLE encodeAndStore #-}

-- | Combinator to handle the frequent combination of lookup and, if that fails, running an action to update the cache. 
-- TODO: We need some exception handling here to make sure, in the case of an Atomic cache,
-- the TMVar gets filled somehow and the key deleted from cache.
-- NB: This returnss an action with the cache time and another action to get the data.  THis allows us
-- to defer deserialization (and maybe loading??) until we actually want to use the data...
retrieveOrMakeAndUpdateCache
  :: forall k ct r b a. ( P.Members [Cache k ct, P.Embed IO] r
     ,  K.LogWithPrefixesLE r
     , Show k
     )
  => Serialize r a ct -- serialization/deserialization
  -> (b -> P.Sem r (Maybe a)) -- action to run to make @a@ if cache is empty or expired, uses deps in b
  -> k
  -> ActionWithCacheTime r b  -- oldest data we will accept.  E.g., cache has data but it's older than its newest dependency, we rebuild.
  -> P.Sem r (Maybe (ActionWithCacheTime r a))
retrieveOrMakeAndUpdateCache (Serialize encode decode encBytes) tryIfMissing key (WithCacheTime newestM bA) =
  K.wrapPrefix ("AtomicCache.retrieveOrMakeAndUpdateCache (key=" <> (T.pack $ show key) <> ")") $ do
    let
      makeAndUpdate :: P.Sem r (Maybe (ActionWithCacheTime r a))
      makeAndUpdate = do
        K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": Trying to make from given action."
        K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": running actions for dependencies."
        b <- bA
        K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": making new item."
        ma <- tryIfMissing b
        case ma of
          Nothing -> do
            K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": Making failed."
            cacheUpdate key Nothing
            return Nothing
          Just a -> do
            K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": Making/Encoding..."
            (ct', a') <- encode a -- a' is the buffered version of a (if necessary)
            let nBytes = encBytes ct'
            K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": serialized to " <> (T.pack $ show nBytes) <> " bytes."
            K.logLE K.Diagnostic $ "Updating cache..."          
            cacheUpdate key (Just ct') 
            curTime <- P.embed Time.getCurrentTime -- Should this come from the cache so the times are the same?  Or is it safe enough that this is later?
            K.logLE K.Diagnostic $ "Finished making and updating."          
            return $ Just (WithCacheTime (Just curTime) (return a'))
    fromCache <- cacheLookup key
    case fromCache of
      Just (WithCacheTime cTimeM mct) -> do
        if cTimeM >= newestM --maybe True (\newest -> cTimeM > newest) newestM
          then do
            let ct = runIdentity mct -- we do this out here only because we want the length.  We could defer this unpacking to the decodeAction
            let nBytes = encBytes ct
            K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": Retrieved " <> (T.pack $ show nBytes) <> " bytes from cache."
            let decodeAction :: P.Sem r a
                decodeAction = do
                   K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": deserializing."  
                   a <- decode ct -- a <- mct >>= decode
                   K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": deserializing complete."  
                   return a
            return (Just $ WithCacheTime cTimeM decodeAction)             
          else do
            K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": Item in cache too old. Making new."
            makeAndUpdate
      Nothing -> do
        K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": Item not found in cache. Making new."
        makeAndUpdate
{-# INLINEABLE retrieveOrMakeAndUpdateCache #-}  

-- | Combinator to combine the action of retrieving from cache and deserializing
-- | throws if item not found or any other error during retrieval
retrieveAndDecode
  :: (P.Member (Cache k ct) r
     , P.Member (P.Embed IO) r
     , P.MemberWithError (P.Error CacheError) r
     , K.LogWithPrefixesLE r
     , Show k
     )
  => Serialize r a ct
  -> k
  -> Maybe Time.UTCTime
  -> P.Sem r (ActionWithCacheTime r a)
retrieveAndDecode s k newestM = K.wrapPrefix ("AtomicCache.retrieveAndDecode (key=" <> (T.pack $ show k) <> ")") $ do
  fromCache <- retrieveOrMakeAndUpdateCache s (const $ return Nothing) k (onlyCacheTime newestM)
  case fromCache of
    Nothing -> P.throw $ ItemNotFoundError $ "No item found/item too old for key=" <> (T.pack $ show k) <> "."
    Just x -> return x
{-# INLINEABLE retrieveAndDecode #-}

-- | Combinator to combine the action of retrieving from cache and deserializing
-- | Returns @Nothing@ if item not found and throws on any other error.
lookupAndDecode
  :: forall k a ct r
   . ( P.Member (Cache k ct) r
     , K.LogWithPrefixesLE r
     , P.Member (P.Embed IO) r
     , P.MemberWithError (P.Error CacheError) r
     , Show k
     )
  => Serialize r a ct
  -> k
  -> Maybe Time.UTCTime
  -> P.Sem r (Maybe (ActionWithCacheTime r a))
lookupAndDecode s k newestM = K.wrapPrefix ("AtomicCache.lookupAndDecode (key=" <> (T.pack $ show k) <> ")")
                              $ retrieveOrMakeAndUpdateCache s (const $ return Nothing) k (onlyCacheTime newestM)
{-# INLINEABLE lookupAndDecode #-}

-- | Combinator to combine the action of retrieving from cache and deserializing
-- | Throws if item not found and making fails.
retrieveOrMake
  :: ( P.Member (Cache k ct) r
     , K.LogWithPrefixesLE r
     , P.Member (P.Embed IO) r
     , P.MemberWithError (P.Error CacheError) r
     , Show k
     )
  => Serialize r a ct
  -> k
  -> ActionWithCacheTime r b
  -> (b -> P.Sem r a)
  -> P.Sem r (ActionWithCacheTime r a)
retrieveOrMake s key cachedDeps makeAction = K.wrapPrefix ("retrieveOrMake (key=" <> (T.pack $ show key) <> ")") $ do
  let makeIfMissing x = K.wrapPrefix "retrieveOrMake.makeIfMissing" $ do
        K.logLE K.Diagnostic $ "Item (at key=" <> (T.pack $ show key) <> ") not found/too old. Making..."
        fmap Just $ makeAction x
  fromCache <- retrieveOrMakeAndUpdateCache s makeIfMissing key cachedDeps 
  case fromCache of
    Just x -> return x
    Nothing -> P.throw $ OtherCacheError $ "retrieveOrMake returned with Nothing.  Which should be impossible, unless called with action which produced Nothing."

-- | Combinator for clearing the cache at a given key
clear :: P.Member (Cache k ct) r => k -> P.Sem r ()
clear k = cacheUpdate k Nothing
{-# INLINEABLE clear #-}

-- | Combinator for clearing the cache at a given key, dosn't throw on IOError
clearIfPresent :: (P.Member (Cache k ct) r, P.MemberWithError (P.Error CacheError) r) => k -> P.Sem r ()
clearIfPresent k = cacheUpdate k Nothing `P.catch` (\(_ :: CacheError) -> return ())
{-# INLINEABLE clearIfPresent #-}

-- structure for in-memory atomic cache
-- outer TVar so only one thread can get the inner TMVar at a time
-- TMVar so we can block if mulitple threads are trying to read or update
-- Maybe inside so we can notify waiting threads that whatever they were waiting on
-- to fill the TMVar failed.

  
type AtomicMemCache k v = C.TVar (M.Map k (C.TMVar (Maybe (WithCacheTime Identity v))))

-- | lookup combinator for in-memory AtomicMemCache
atomicMemLookup :: (Ord k
                   , Show k
                   , P.Member (P.Embed IO) r
                   , K.LogWithPrefixesLE r
                   )
              => AtomicMemCache k ct
              -> k
              -> P.Sem r (Maybe (WithCacheTime Identity ct))
atomicMemLookup cache key = K.wrapPrefix "atomicMemLookup" $ do
  K.logLE K.Diagnostic $ "key=" <> (T.pack $ show key) <> ": Called."
  P.embed $ C.atomically $ do
    mv <- (C.readTVar cache >>= fmap join . traverse C.readTMVar . M.lookup key)
    case mv of
      Just wctv -> return $ Just wctv
      Nothing -> do
        newTMV <- C.newEmptyTMVar
        C.modifyTVar cache (M.insert key newTMV)
        return Nothing
{-# INLINEABLE atomicMemLookup #-}

-- | data type to simplify logging in AtomicMemCache updates
data MemUpdateAction = Deleted | Replaced | Filled deriving (Show)

-- | update combinator for in-memory AtomicMemCache
atomicMemUpdate :: (Ord k
                   , Show k
                   , P.Member (P.Embed IO) r
                   , K.LogWithPrefixesLE r
                   )
                => AtomicMemCache k ct
                -> k
                -> Maybe ct
                -> P.Sem r ()
atomicMemUpdate cache key mct =
  K.wrapPrefix "atomicMemUpdate" $ do
  let keyText = "key=" <> (T.pack $ show key) <> ": "
  K.logLE K.Diagnostic $ keyText <> "called."
  updateAction <- case mct of
    Nothing -> (P.embed $ C.atomically $ C.modifyTVar cache (M.delete key)) >> return Deleted
    Just ct -> do
      curTime <- P.embed Time.getCurrentTime
      let wct = WithCacheTime (Just curTime) (Identity ct)
      P.embed $ C.atomically $ do
        m <- C.readTVar cache
        case M.lookup key m of
          Nothing -> do
            newTMV <- C.newTMVar (Just wct)
            C.modifyTVar cache (M.insert key newTMV)
            return Filled
          Just tmvM -> do
            wasEmptyTMVar <- C.tryPutTMVar tmvM (Just wct)
            if wasEmptyTMVar
              then return Filled
              else (C.swapTMVar tmvM (Just wct)) >> return Replaced
  case updateAction of
    Deleted -> K.logLE K.Diagnostic $ keyText <> "deleted"
    Replaced -> K.logLE K.Diagnostic $ keyText <> "replaced"
    Filled -> K.logLE K.Diagnostic $ keyText <> "filled"
{-# INLINEABLE atomicMemUpdate #-}

-- | Interpreter for in-memory only AtomicMemCache
runAtomicInMemoryCache :: (Ord k
                          , Show k
                          , P.Member (P.Embed IO) r
                          , K.LogWithPrefixesLE r
                          )
                       => AtomicMemCache k ct
                       -> P.InterpreterFor (Cache k ct) r
runAtomicInMemoryCache cache =
  P.interpret $ \case
    CacheLookup key -> atomicMemLookup cache key
    CacheUpdate key mct -> atomicMemUpdate cache key mct
{-# INLINEABLE runAtomicInMemoryCache #-}


-- Backed by Another Cache
-- lookup is the hard case.  If we don't find it, we want to check the backup cache
-- and fill in this cache from there, if possible
-- | lookup for an AtomicMemCache which is backed by some other cache, probably a persistence layer.
atomicMemLookupB :: (Ord k
                    , P.Members '[P.Embed IO, Cache k ct] r
                    , K.LogWithPrefixesLE r
                    , Show k
                    )
                 =>  AtomicMemCache k ct
                 -> k
                 -> P.Sem r (Maybe (WithCacheTime Identity ct))
atomicMemLookupB cache key = K.wrapPrefix "atomicMemLookupB" $ do
  let keyText = "key=" <> (T.pack $ show key) <> ": "
  K.logLE K.Diagnostic $ keyText <> "checking in mem cache..."
  x <- P.embed $ C.atomically $ do
    mTMV <- M.lookup key <$> C.readTVar cache
    case mTMV of
      Just tmv -> do
        mv <- C.takeTMVar tmv  
        case mv of
          Just wct -> do -- in cache with value (and time)
            C.putTMVar tmv (Just wct)
            return $ Right wct
          Nothing -> return $ Left tmv  -- in cache but set to Nothing
      Nothing -> do -- not found
        newTMV <- C.newEmptyTMVar
        C.modifyTVar cache (M.insert key newTMV)
        return $ Left newTMV
  case x of
    Right wct -> K.logLE K.Diagnostic (keyText <> "found.") >> return (Just wct)
    Left emptyTMV -> do
      K.logLE K.Diagnostic (keyText <> "not found.  Holding empty TMVar. Checking backup cache...")
      inOtherM <- cacheLookup key      
      case inOtherM of
        Nothing -> K.logLE K.Diagnostic (keyText <> "not found in backup cache.") >> return Nothing
        Just (WithCacheTime tM mct) -> do
          K.logLE K.Diagnostic (keyText <> "Found in backup cache.  Filling empty TMVar.")
          let ct = runIdentity mct
          P.embed $ C.atomically $ C.putTMVar emptyTMV (Just $ WithCacheTime tM (Identity ct)) 
          K.logLE K.Diagnostic (keyText <> "Returning")
          return $ Just $ WithCacheTime tM (pure ct) 
{-# INLINEABLE atomicMemLookupB #-}

-- | update for an AtomicMemCache which is backed by some other cache, probably a persistence layer.
-- This just does the update in both caches
atomicMemUpdateB ::  (Ord k
                     , Show k
                     , K.LogWithPrefixesLE r
                     , P.Members '[P.Embed IO, Cache k ct] r)
                 => AtomicMemCache k ct
                 -> k
                 -> Maybe ct
                 -> P.Sem r ()
atomicMemUpdateB cache key mct = K.wrapPrefix "atomicMemUpdateB" $ do
  let keyText = "key=" <> (T.pack $ show key) <> ": "
  K.logLE K.Diagnostic $ keyText <> "Calling atomicMemUpdate"
  atomicMemUpdate cache key mct
  K.logLE K.Diagnostic $ keyText <> "Calling cacheUpdate in backup cache."
  cacheUpdate key mct
{-# INLINEABLE atomicMemUpdateB #-}

-- | interpret Cache via a different-Cache-backed AtomicMemCache
runBackedAtomicInMemoryCache :: (Ord k
                                , Show k
                                , K.LogWithPrefixesLE r
                                , P.Members '[P.Embed IO, Cache k ct] r
                                )
                             => AtomicMemCache k ct
                             -> P.InterpreterFor (Cache k ct) r
runBackedAtomicInMemoryCache cache =
  P.interpret $ \case
    CacheLookup k -> atomicMemLookupB cache k
    CacheUpdate k mct -> atomicMemUpdateB cache k mct
{-# INLINEABLE runBackedAtomicInMemoryCache #-}

-- | re-interpret Cache, using AtomicMemCache for in-memory store, into another cache, usually a persistent store.
backedAtomicInMemoryCache :: (Ord k
                             , Show k
                             , P.Member (P.Embed IO) r
                             , K.LogWithPrefixesLE r
                             )
                          => AtomicMemCache k ct
                          -> P.Sem ((Cache k ct) ': r) a
                          -> P.Sem ((Cache k ct) ': r) a
backedAtomicInMemoryCache cache =
  P.reinterpret $ \case
    CacheLookup k -> atomicMemLookupB cache k
    CacheUpdate k mct -> atomicMemUpdateB cache k mct    
{-# INLINEABLE backedAtomicInMemoryCache #-} 


-- | Interpret Cache via AtomicMemCache and an interpreter for a backing cache,
-- usually a persistence layer.
runPersistenceBackedAtomicInMemoryCache :: (Ord k
                                           , Show k
                                           , P.Member (P.Embed IO) r
                                           , P.MemberWithError (P.Error CacheError) r
                                           , K.LogWithPrefixesLE r
                                           )
                                        => P.InterpreterFor (Cache k ct) r -- persistence layer interpreter
                                        -> AtomicMemCache k ct
                                        -> P.InterpreterFor (Cache k ct) r
runPersistenceBackedAtomicInMemoryCache runPersistentCache cache = runPersistentCache . backedAtomicInMemoryCache cache
{-# INLINEABLE runPersistenceBackedAtomicInMemoryCache #-}

-- | Interpret Cache via AtomicMemCache and an interpreter for a backing cache,
-- usually a persistence layer.  Create a new, empty, AtomicMemCache to begin.
runPersistenceBackedAtomicInMemoryCache' :: (Ord k
                                            , Show k
                                            , P.Member (P.Embed IO) r
                                            , P.MemberWithError (P.Error CacheError) r
                                            , K.LogWithPrefixesLE r
                                            )
                                        => P.InterpreterFor (Cache k ct) r
                                        -> P.InterpreterFor (Cache k ct) r
runPersistenceBackedAtomicInMemoryCache' runPersistentCache x = do
  cache <- P.embed $ C.atomically $ C.newTVar mempty
  runPersistenceBackedAtomicInMemoryCache runPersistentCache cache x 


-- | Interpreter for Cache via persistence to disk as a Streamly Memory.Array (Contiguous storage of Storables) of Bytes (Word8)
persistAsByteArray
  :: (Show k, P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> P.InterpreterFor (Cache k (Streamly.Array.Array Word.Word8)) r
persistAsByteArray keyToFilePath =
  P.interpret $ \case
    CacheLookup k -> K.wrapPrefix "persistAsByteArray.CacheLookup" $ do
      let filePath = keyToFilePath k
      getContentsWithCacheTime (Streamly.Array.fromStream . Streamly.File.toBytes) filePath
    CacheUpdate k mct -> K.wrapPrefix "persistAsByteStreamly.CacheUpdate" $ do
      let keyText = "key=" <> (T.pack $ show k) <> ": "
      case mct of
        Nothing -> do
           K.logLE K.Diagnostic $ keyText <> "called with Nothing. Deleting file."
           rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
        Just ct -> do
          K.logLE K.Diagnostic $ keyText <> "called with content. Writing file."
          let filePath     = (keyToFilePath k)
              (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
          _ <- createDirIfNecessary dirPath
          K.logLE K.Diagnostic $ "Writing serialization to disk."
          K.logLE K.Diagnostic $ keyText <> "Writing " <> (T.pack $ show $ Streamly.Array.length ct) <> " bytes to disk." 
          rethrowIOErrorAsCacheError $ Streamly.File.writeArray filePath ct
{-# INLINEABLE persistAsByteArray #-}

-- | Interpreter for Cache via persistence to disk as a strict ByteString
persistAsStrictByteString
  :: (P.Members '[P.Embed IO] r, P.MemberWithError (P.Error CacheError) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> P.InterpreterFor (Cache k BS.ByteString) r
persistAsStrictByteString keyToFilePath =
  P.interpret $ \case
    CacheLookup k -> getContentsWithCacheTime BS.readFile (keyToFilePath k)
    CacheUpdate k mct -> case mct of
      Nothing -> rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
      Just ct -> do
        let filePath     = (keyToFilePath k)
            (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
        _ <- createDirIfNecessary dirPath
        K.logLE K.Diagnostic $ "Writing serialization to disk."
        rethrowIOErrorAsCacheError $ BS.writeFile filePath ct  -- maybe we should do this in another thread?
{-# INLINEABLE persistAsStrictByteString #-}

-- | Interpreter for Cache via persistence to disk as a lazy ByteString
persistAsByteString
  :: (P.Members '[P.Embed IO] r, P.MemberWithError (P.Error CacheError) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> P.InterpreterFor (Cache k BL.ByteString) r
persistAsByteString keyToFilePath =
  P.interpret $ \case
    CacheLookup k -> getContentsWithCacheTime BL.readFile (keyToFilePath k)
    CacheUpdate k mct -> case mct of
      Nothing -> rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
      Just ct -> do
        let filePath     = (keyToFilePath k)
            (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
        _ <- createDirIfNecessary dirPath
        K.logLE K.Diagnostic $ "Writing serialization to disk."
        rethrowIOErrorAsCacheError $ BL.writeFile filePath ct  -- maybe we should do this in another thread?

-- | Interpreter Cache via persistence to disk as a Streamly stream of Bytes (Word8)
persistAsByteStreamly
  :: (Show k, P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> P.InterpreterFor (Cache k (Streamly.SerialT Identity Word.Word8)) r
persistAsByteStreamly keyToFilePath =
  P.interpret $ \case
    CacheLookup k -> K.wrapPrefix "persistAsByteStreamly" $ getContentsWithCacheTime (sequenceStreamly . Streamly.File.toBytes) (keyToFilePath k)
    CacheUpdate k mct -> K.wrapPrefix "persistAsByteStreamly.CacheUpdate" $ do
      let keyText = "key=" <> (T.pack $ show k) <> ": "
      case mct of
        Nothing -> do
          K.logLE K.Diagnostic $ keyText <> "called with Nothing. Deleting file."
          rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
        Just ct -> do
          K.logLE K.Diagnostic $ keyText <> "called with content. Writing file."
          let filePath     = (keyToFilePath k)
              (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
          _ <- createDirIfNecessary dirPath
          K.logLE K.Diagnostic $ keyText <> "Writing serialization to disk."
          let sLength = runIdentity $ Streamly.length ct
          K.logLE K.Diagnostic $ keyText <> "Writing " <> (T.pack $ show sLength) <> " bytes to disk." 
          rethrowIOErrorAsCacheError $ (System.withBinaryFile filePath System.WriteMode $ writeToHandle ct) -- maybe we should do this in another thread?
  where
    sequenceStreamly :: Monad m => Streamly.SerialT m Word.Word8 -> m (Streamly.SerialT Identity Word.Word8)
    sequenceStreamly = fmap Streamly.fromList . Streamly.toList
    streamlyRaise :: Monad m => Streamly.SerialT Identity Word.Word8 -> Streamly.SerialT m Word.Word8
    streamlyRaise = Streamly.fromList . runIdentity . Streamly.toList
    writeToHandle bs h = Streamly.fold (Streamly.Handle.write h) $ streamlyRaise bs
{-# INLINEABLE persistAsByteStreamly #-}

createDirIfNecessary
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => T.Text
  -> K.Sem r ()
createDirIfNecessary dir = K.wrapPrefix "createDirIfNecessary" $ do
  K.logLE K.Diagnostic $ "Checking if cache path (\"" <> dir <> "\") exists."
  existsB <- P.embed $ (System.doesDirectoryExist (T.unpack dir))
  case existsB of
    True -> do
      K.logLE K.Diagnostic $ "\"" <> dir <> "\" exists."
      return ()
    False -> do
      K.logLE K.Info
        $  "Cache directory (\""
        <> dir
        <> "\") not found. Atttempting to create."
      P.embed
        $ System.createDirectoryIfMissing True (T.unpack dir)
{-# INLINEABLE createDirIfNecessary #-}


getContentsWithCacheTime :: (P.Members '[P.Embed IO] r
                            , P.MemberWithError (P.Error CacheError) r
                            , K.LogWithPrefixesLE r)
                         => (FilePath -> IO a)
                         -> FilePath
                         -> P.Sem r (Maybe (WithCacheTime Identity a))
getContentsWithCacheTime f fp =  K.wrapPrefix "getContentsWithCacheTime" $ do
  K.logLE K.Diagnostic $ "Reading serialization from disk."
  rethrowIOErrorAsCacheError $ fileNotFoundToMaybe $ do
    ct <- f fp
    cTime <- System.getModificationTime fp
    return $ WithCacheTime (Just cTime) (Identity ct)

fileNotFoundToEither :: IO a -> IO (Either () a)
fileNotFoundToEither x = (fmap Right x) `Exception.catch` f where
  f :: Exception.IOException -> IO (Either () a)
  f e = if IO.Error.isDoesNotExistError e then return (Left ()) else Exception.throw e 
{-# INLINEABLE fileNotFoundToEither #-}

fileNotFoundToMaybe :: IO a -> IO (Maybe a)
fileNotFoundToMaybe x = (fmap Just x) `Exception.catch` f where
  f :: Exception.IOException -> IO (Maybe a)
  f e = if IO.Error.isDoesNotExistError e then return Nothing else Exception.throw e 
{-# INLINEABLE fileNotFoundToMaybe #-}


rethrowIOErrorAsCacheError :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r) => IO a -> P.Sem r a
rethrowIOErrorAsCacheError x = P.fromExceptionVia (\(e :: IO.Error.IOError) -> PersistError $ "IOError: " <> (T.pack $ show e)) x



