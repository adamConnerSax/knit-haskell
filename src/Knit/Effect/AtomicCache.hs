{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
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
    -- * Actions
  , cacheLookup
  , cacheUpdate
  , encodeAndStore
  , retrieveAndDecode
  , lookupAndDecode
  , clear
    -- * Serialization
  , Serialize(..)
    -- * Persistance
  , Persist(..)
  , persistAsByteString
  , persistAsStrictByteString
  , persistAsByteStreamly
  , persistAsByteArray
    -- * Interpretations
  , runPersistentCache
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
import qualified Polysemy.AtomicState          as P
import qualified Knit.Effect.Logger            as K

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor.Identity          (Identity(..))
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Word                     as Word

import qualified Control.Concurrent.STM        as C
import qualified Control.Exception             as Exception
--import qualified Control.Monad.Exception       as MTL.Exception
--import           Control.Monad                  ( join )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )

import qualified Streamly                      as Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Memory.Array         as Streamly.Array
import qualified Streamly.FileSystem.Handle    as Streamly.Handle

import qualified System.IO                     as System
import qualified System.Directory              as System
import qualified System.IO.Error               as IO.Error

data CacheError =
  ItemNotFoundError T.Text
  | DeserializationError T.Text
  | PersistError T.Text deriving (Show, Eq)

data Serialize r a ct where
  Serialize :: (P.MemberWithError (P.Error CacheError) r)
            => (a -> ct) -> (ct -> P.Sem r a) -> (ct -> Int) -> Serialize r a ct

-- | This is a Key/Value store
-- | Tagged by @t@ so we can have more than one for the same k and v
data Cache k v m a where
  CacheLookup :: k -> Cache k v m (Maybe v)
  CacheUpdate :: k -> Maybe v -> Cache k v m ()

P.makeSem ''Cache
{-
eitherThrow :: P.MemberWithError (P.Error e) r => P.Sem r (Either e a) -> P.Sem r a
eitherThrow x = do
  ea <- x
  case ea of
    Left  e -> P.throw e
    Right a -> return a --fmap (either (P.throw @e) id)
{-# INLINEABLE eitherThrow #-}

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just
{-# INLINEABLE hush #-}
-}

-- | Combinator to combine the action of serializing and caching
encodeAndStore
  :: (Show k
     , P.Member (Cache k ct) r
     ,  K.LogWithPrefixesLE r)
  => Serialize r a ct
  -> k
  -> a
  -> P.Sem r ()
encodeAndStore (Serialize encode _ encBytes) k x = K.wrapPrefix "Knit.AtomicCache.store" $ do
  K.logLE K.Diagnostic $ "encoding (serializing) data for key=" <> (T.pack $ show k) 
  let encoded = encode x
  K.logLE K.Diagnostic $ "Storing " <> (T.pack $ show $ encBytes encoded) <> " bytes of encoded data in cache for key=" <> (T.pack $ show k) 
  cacheUpdate k (Just encoded)
{-# INLINEABLE encodeAndStore #-}


-- | Combinator to combine the action of retrieving from cache and deserializing
-- | throws if item not found or any other error during retrieval
retrieveAndDecode
  :: (P.Member (Cache k ct) r
     , P.MemberWithError (P.Error CacheError) r
     , K.LogWithPrefixesLE r
     , Show k
     )
  => Serialize r a ct
  -> k
  -> P.Sem r a
retrieveAndDecode (Serialize _ decode encBytes) k = do
  fromCache <- cacheLookup k
  case fromCache of
    Nothing -> P.throw $ ItemNotFoundError $ "No item found in cache for key=" <> (T.pack $ show k) <> "."
    Just x -> do
      K.logLE K.Diagnostic $ "Retrieved " <> (T.pack $ show $ encBytes x) <> " bytes from cache. Deserializing..."
      decode x
{-# INLINEABLE retrieveAndDecode #-}

-- | Combinator to combine the action of retrieving from cache and deserializing
-- | Returns @Nothing@ if item not found and throws on any other error.
lookupAndDecode
  :: forall e1 e2 k a ct r
   . ( P.Member (Cache k ct) r
     , K.LogWithPrefixesLE r
     )
  => Serialize r a ct
  -> k
  -> P.Sem r (Maybe a)
lookupAndDecode (Serialize _ decode encBytes) k = do
  let reportAndDecode x = do
        K.logLE K.Diagnostic $ "Retrieved " <> (T.pack $ show $ encBytes x) <> " bytes from cache. Deserializing..."
        decode x
  cacheLookup k >>= traverse reportAndDecode
{-# INLINEABLE lookupAndDecode #-}


-- | Combinator for clearing the cache at a given key
clear :: P.Member (Cache k ct) r => k -> P.Sem r ()
clear k = cacheUpdate k Nothing
{-# INLINEABLE clear #-}

-- | Data type to hold the persistence functions of:
-- reading from the store
-- writing to the store
-- clearing the store
-- each of which can throw errors of type @CacheError@
data Persist r k ct where
  Persist :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r)
          => (k -> P.Sem r (Maybe ct))
          -> (k -> ct -> P.Sem r ())
          -> (k -> P.Sem r ())
          -> Persist r k ct

pLookup :: Persist r k ct -> k -> P.Sem r (Maybe ct)
pLookup (Persist lookupD _ _) = lookupD

pStore :: Persist r k ct -> k -> ct -> P.Sem r ()
pStore  (Persist _ storeD _) = storeD

pClear :: Persist r k ct -> k -> P.Sem r ()
pClear  (Persist _ _ clearD) = clearD

runPersistentCache :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r)
                   => Persist r k ct -> P.InterpreterFor (Cache k ct) r
runPersistentCache p =
  P.interpret $ \case
    CacheLookup k -> pLookup p k
    CacheUpdate k mct -> case mct of
      Nothing -> pClear p k
      Just ct -> pStore p k ct

-- structure for in-memory cache
-- outer TVar so only one thread can get the inner TMVar at a time
-- TMVar so we can block if mulitple threads are trying to read or update
type AtomicMemCache k v = C.TVar (M.Map k (C.TMVar v))

-- interpret via MemCache
atomicMemLookup :: (Ord k, P.Member (P.Embed IO) r)
              => AtomicMemCache k ct
              -> k
              -> P.Sem r (Maybe ct) -- we either return the value or a tvar to be filled
atomicMemLookup cache key = P.embed $ C.atomically $ (C.readTVar cache >>= traverse C.readTMVar . M.lookup key)
{-# INLINEABLE atomicMemLookup #-}

atomicMemUpdate :: (Ord k, P.Member (P.Embed IO) r)
                => AtomicMemCache k ct
                -> k
                -> Maybe ct
                -> P.Sem r ()
atomicMemUpdate cache key mct = P.embed $ C.atomically $ do
  case mct of
    Nothing -> C.modifyTVar cache (M.delete key) 
    Just ct -> do
      m <- C.readTVar cache
      case M.lookup key m of
        Nothing -> do
          newTMV <- C.newTMVar ct
          C.modifyTVar cache (M.insert key newTMV)
        Just tmv -> C.putTMVar tmv ct
{-# INLINEABLE atomicMemUpdate #-}

runAtomicInMemoryCache :: (Ord k, P.Member (P.Embed IO) r) => AtomicMemCache k ct -> P.InterpreterFor (Cache k ct) r
runAtomicInMemoryCache cache =
  P.interpret $ \case
    CacheLookup k -> atomicMemLookup cache k
    CacheUpdate k mct -> atomicMemUpdate cache k mct
{-# INLINEABLE runAtomicInMemoryCache #-}

-- Backed by Another Cache
-- lookup is the hard case.  If we don't find it, we want to check the backup cache
-- and fill in this cache from there, if possible
atomicMemLookupB :: (Ord k, P.Members '[P.Embed IO, Cache k ct] r)
                 =>  AtomicMemCache k ct
                 -> k
                 -> P.Sem r (Maybe ct) 
atomicMemLookupB cache key = do
  x <- P.embed $ C.atomically $ do
    mTMV <- M.lookup key <$> C.readTVar cache
    case mTMV of
      Just x -> fmap Right $ C.readTMVar x -- already in memory so get/wait for it
      Nothing -> do
        newTMV <- C.newEmptyTMVar
        C.modifyTVar cache (M.insert key newTMV)
        return $ Left newTMV
  case x of
    Right ct -> return $ Just ct
    Left emptyTMV -> do
      inOtherM <- cacheLookup key
      case inOtherM of
        Nothing -> do
          P.embed $ C.atomically $ C.modifyTVar cache (M.delete key)
          return Nothing
        Just ct -> do
          P.embed $ C.atomically $ C.putTMVar emptyTMV ct
          return $ Just ct
{-# INLINEABLE atomicMemLookupB #-}
            
atomicMemUpdateB ::  (Ord k, P.Members '[P.Embed IO, Cache k ct] r)
                 => AtomicMemCache k ct
                 -> k
                 -> Maybe ct
                 -> P.Sem r ()
atomicMemUpdateB cache key mct = do
  atomicMemUpdate cache key mct
  cacheUpdate key mct
{-# INLINEABLE atomicMemUpdateB #-}

runBackedAtomicInMemoryCache :: (Ord k, P.Members '[P.Embed IO, Cache k ct] r)
                             => AtomicMemCache k ct
                             -> P.InterpreterFor (Cache k ct) r
runBackedAtomicInMemoryCache cache =
  P.interpret $ \case
    CacheLookup k -> atomicMemLookupB cache k
    CacheUpdate k mct -> atomicMemUpdateB cache k mct
{-# INLINEABLE runBackedAtomicInMemoryCache #-}

backedAtomicInMemoryCache :: (Ord k, P.Member (P.Embed IO) r)
                          => AtomicMemCache k ct
                          -> P.Sem ((Cache k ct) ': r) a
                          -> P.Sem ((Cache k ct) ': r) a
backedAtomicInMemoryCache cache =
  P.reinterpret $ \case
    CacheLookup k -> atomicMemLookupB cache k
    CacheUpdate k mct -> atomicMemUpdateB cache k mct    
{-# INLINEABLE backedAtomicInMemoryCache #-} 

runPersistenceBackedAtomicInMemoryCache :: (Ord k, P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r)
                                        => Persist r k ct
                                        -> AtomicMemCache k ct
                                        -> P.InterpreterFor (Cache k ct) r
runPersistenceBackedAtomicInMemoryCache p cache =
  runPersistentCache p . backedAtomicInMemoryCache cache
{-# INLINEABLE runPersistenceBackedAtomicInMemoryCache #-}

runPersistenceBackedAtomicInMemoryCache' :: (Ord k, P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r)
                                        => Persist r k ct
                                        -> P.InterpreterFor (Cache k ct) r
runPersistenceBackedAtomicInMemoryCache' p x = do
  cache <- P.embed $ C.atomically $ C.newTVar mempty
  runPersistenceBackedAtomicInMemoryCache p cache x 



persistAsByteStreamly
  :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist r k (Streamly.SerialT Identity Word.Word8)
persistAsByteStreamly keyToFilePath = Persist readBA writeBA deleteFile
 where
  sequenceStreamly :: Monad m => Streamly.SerialT m Word.Word8 -> m (Streamly.SerialT Identity Word.Word8)
  sequenceStreamly = fmap Streamly.fromList . Streamly.toList
  streamlyRaise :: Monad m => Streamly.SerialT Identity Word.Word8 -> Streamly.SerialT m Word.Word8
  streamlyRaise = Streamly.fromList . runIdentity . Streamly.toList
  readFromHandle h = sequenceStreamly $ Streamly.unfold Streamly.Handle.read h
  writeToHandle bs h = Streamly.fold (Streamly.Handle.write h) $ streamlyRaise bs
  readBA k = do
    let filePath = keyToFilePath k
    K.logLE K.Diagnostic $ "Reading serialization from disk."
    rethrowIOErrorAsCacheError $ fileNotFoundToNothing $ System.withBinaryFile filePath System.ReadMode readFromHandle
  writeBA k bs = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    K.logLE K.Diagnostic $ "Writing " <> (T.pack $ show $ Streamly.length bs) <> " bytes to disk." 
    rethrowIOErrorAsCacheError $ (System.withBinaryFile filePath System.WriteMode $ writeToHandle bs) -- maybe we should do this in another thread?
  deleteFile k = rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
{-# INLINEABLE persistAsByteStreamly #-}


persistAsByteArray
  :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist r k (Streamly.Array.Array Word.Word8)
persistAsByteArray keyToFilePath = Persist readBA writeBA deleteFile
 where
  readArrayFromHandle h = Streamly.fold Streamly.Array.write $ Streamly.unfold Streamly.Handle.read h
  writeArrayToHandle ba h = Streamly.fold (Streamly.Handle.write h) $ Streamly.unfold Streamly.Array.read ba
  readBA k = do
    let filePath = keyToFilePath k
    K.logLE K.Diagnostic $ "Reading serialization from disk."
    rethrowIOErrorAsCacheError $ fileNotFoundToNothing $ System.withFile filePath System.ReadMode readArrayFromHandle
  writeBA k ba = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    rethrowIOErrorAsCacheError $ System.withFile filePath System.WriteMode $ writeArrayToHandle ba -- should we do on another thread??
  deleteFile k = rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
{-# INLINEABLE persistAsByteArray #-}


persistAsStrictByteString
  :: (P.Members '[P.Embed IO] r, P.MemberWithError (P.Error CacheError) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist r k BS.ByteString
persistAsStrictByteString keyToFilePath = Persist readBS writeBS deleteFile
 where
  readBS k = do
    K.logLE K.Diagnostic $ "Reading serialization from disk."
    rethrowIOErrorAsCacheError $ fileNotFoundToNothing $ BS.readFile (keyToFilePath k)
  writeBS k !b = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    rethrowIOErrorAsCacheError $ BS.writeFile filePath b  -- maybe we should do this in another thread?
  deleteFile k = rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
{-# INLINEABLE persistAsStrictByteString #-}

-- | Persist functions for disk-based persistence with a lazy ByteString interface on the serialization side
persistAsByteString
  :: (P.Members '[P.Embed IO] r, P.MemberWithError (P.Error CacheError) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist r k BL.ByteString
persistAsByteString keyToFilePath = Persist readBL writeBL deleteFile
 where
  readBL k = do
    K.logLE K.Diagnostic $ "Reading serialization from disk."
    rethrowIOErrorAsCacheError $ fileNotFoundToNothing $ BL.readFile (keyToFilePath k)
  writeBL k bs = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk:"
    K.logLE K.Diagnostic $ "Bytestring is " <> (T.pack $ show $ BL.length bs) <> " bytes."
    rethrowIOErrorAsCacheError $ BL.writeFile filePath bs -- maybe we should do this in another thread?
  deleteFile k = rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
{-# INLINEABLE persistAsByteString #-}

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


fileNotFoundToNothing :: IO a -> IO (Maybe a)
fileNotFoundToNothing x = (fmap Just x) `Exception.catch` f where
  f :: Exception.IOException -> IO (Maybe a)
  f e = if IO.Error.isDoesNotExistError e then return Nothing else Exception.throw e 
{-# INLINEABLE fileNotFoundToNothing #-}

rethrowIOErrorAsCacheError :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error CacheError) r) => IO a -> P.Sem r a
rethrowIOErrorAsCacheError x = P.fromExceptionVia (\(e :: IO.Error.IOError) -> PersistError $ "IOError: " <> (T.pack $ show e)) x
