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
  , atomicRetrieve
  , atomicUpdate
  , store
  , retrieve
  , retrieveMaybe
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
  , runPersistentAtomicCache
  , runPersistentAtomicCacheC
  , runPersistentAtomicCacheCFromEmpty
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

{-
data Serialize e a ct where
  Serialize :: (Monad (DecodeMonad a), MTL.Exception.MonadError e (DecodeMonad a))
            => (a -> ct) -> (ct -> (DecodeMonad a) a) -> Serialize e a ct
-}
data CacheError = CacheError T.Text -- type for throwing cache errors

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

-- | Combinator to combine the action of serializing and caching
store
  :: (Show k, P.Member (AtomicCache e1 k ct) r, P.MemberWithError (P.Error e1) r,  K.LogWithPrefixesLE r)
  => Serialize r e2 a ct
  -> k
  -> a
  -> P.Sem r ()
store (Serialize encode _) k x = K.wrapPrefix "Knit.AtomicCache.store" $ do
  K.logLE K.Diagnostic $ "encoding (serializing) data for key=" <> (T.pack $ show k) 
  encoded <- encode x
  K.logLE K.Diagnostic $ "Storing encoded data in cache for key=" <> (T.pack $ show k) 
  eitherThrow $ atomicUpdate k (Just encoded)
{-# INLINEABLE store #-}


-- | Combinator to combine the action of retrieving from cache and deserializing
-- NB. Either action may have an error
retrieve
  :: (P.Member (AtomicCache e1 k ct) r, P.MemberWithError (P.Error e1) r , P.MemberWithError (P.Error e2) r)
  => Serialize r e2 a ct
  -> k
  -> P.Sem r a
retrieve (Serialize _ decode) k = eitherThrow (atomicRetrieve k) >>= decode 
{-# INLINEABLE retrieve #-}

retrieveMaybe
  :: forall e1 e2 k a ct r
   . (P.Member (AtomicCache e1 k ct) r, P.MemberWithError (P.Error e2) r)
  => Serialize r e2 a ct
  -> k
  -> P.Sem r (Maybe a)
retrieveMaybe (Serialize _ decode) k = do
  x <- fmap hush (atomicRetrieve k)
  P.catch (traverse decode x) (const $ return Nothing)
{-# INLINEABLE retrieveMaybe #-}
-}

-- | Combinator for clearing the cache at a given key
clear :: P.Member (Cache k ct) r => k -> P.Sem r ()
clear k = cacheUpdate k Nothing
{-# INLINEABLE clear #-}

-- | Data type to hold the persistence functions of:
-- reading from the store
-- writing to the store
-- clearing the store
-- each of which can throw errors of type e
data Persist e r k ct where
  Persist :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error e) r)
          => (k -> P.Sem r (Maybe ct))
          -> (k -> ct -> P.Sem r ())
          -> (k -> P.Sem r ())
          -> Persist e r k ct

pLookup :: Persist e r k ct -> k -> P.Sem r (Maybe ct)
pLookup (Persist lookupD _ _) = lookupD

pStore :: Persist e r k ct -> k -> ct -> P.Sem r ()
pStore  (Persist _ storeD _) = storeD

pClear :: Persist e r k ct -> k -> P.Sem r ()
pClear  (Persist _ _ clearD) = clearD

runPersistentCache :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error e) r)
                   => Persist e r k ct -> P.InterpreterFor (Cache k ct) r
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

runInMemoryCache :: (Ord k, P.Member (P.Embed IO) r) => AtomicMemCache k ct -> P.InterpreterFor (Cache k ct) r
runInMemoryCache cache =
  P.interpret $ \case
    CacheLookup k -> atomicMemLookup cache k
    CacheUpdate k mct -> atomicMemUpdate cache k mct
{-# INLINEABLE runInMemoryCache #-}

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
           
atomicMemUpdateB ::  (Ord k, P.Members '[P.Embed IO, Cache k ct] r)
                 => AtomicMemCache k ct
                 -> k
                 -> Maybe ct
                 -> P.Sem r ()
atomicMemUpdateB cache key mct = do
  atomicMemUpdate cache key mct
  cacheUpdate key mct


runBackedAtomicInMemoryCache :: (Ord k, P.Members '[P.Embed IO, Cache k ct] r)
                             => AtomicMemCache k ct
                             -> P.InterpreterFor (Cache k ct) r
runBackedAtomicInMemoryCache cache =
  P.interpret $ \case
    CacheLookup k -> atomicMemLookupB cache k
    CacheUpdate k mct -> atomicMemUpdateB cache k mct

backedAtomicInMemoryCache :: (Ord k, P.Member (P.Embed IO) r)
                          => AtomicMemCache k ct
                          -> P.Sem ((Cache k ct) ': r) a
                          -> P.Sem ((Cache k ct) ': r) a
backedAtomicInMemoryCache cache =
  P.reinterpret $ \case
    CacheLookup k -> atomicMemLookupB cache k
    CacheUpdate k mct -> atomicMemUpdateB cache k mct    


runPersistenceBackedInMemoryCache :: (Ord k, P.Member (P.Embed IO) r, P.MemberWithError (P.Error e) r)
                                  => Persist e r k ct
                                  -> AtomicMemCache k ct
                                  -> P.InterpreterFor (Cache k ct) r
runPersistenceBackedInMemoryCache p cache =
  runPersistentCache p . backedAtomicInMemoryCache cache
    


persistAsByteStreamly
  :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error Exception.IOException) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist Exception.IOException r k (Streamly.SerialT Identity Word.Word8)
persistAsByteStreamly keyToFilePath = Persist readBA writeBA deleteFile
 where
  sequenceStreamlyIO :: Streamly.SerialT IO Word.Word8 -> IO (Streamly.SerialT Identity Word.Word8)
  sequenceStreamlyIO = fmap Streamly.fromList . Streamly.toList
  toStreamlyIO :: Streamly.SerialT Identity Word.Word8 -> Streamly.SerialT IO Word.Word8
  toStreamlyIO = Streamly.fromList . runIdentity . Streamly.toList
  readFromHandle h = sequenceStreamlyIO $ Streamly.unfold Streamly.Handle.read h
  writeToHandle bs h = Streamly.fold (Streamly.Handle.write h) $ toStreamlyIO bs
  readBA k = do
    let filePath = keyToFilePath k
    K.logLE K.Diagnostic $ "Reading serialization from disk."
    P.embed
      $         fmap Just (System.withBinaryFile filePath System.ReadMode readFromHandle)
      `Exception.catch` (\(e::Exception.IOException) -> return Nothing)
  writeBA k bs = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    K.logLE K.Diagnostic $ "Writing " <> (T.pack $ show $ Streamly.length bs) <> " bytes to disk." 
    liftIO $ (System.withBinaryFile filePath System.WriteMode $ writeToHandle bs) -- maybe we should do this in another thread?
  deleteFile k = liftIO $ System.removeFile (keyToFilePath k)
{-# INLINEABLE persistAsByteStreamly #-}


persistAsByteArray
  :: (P.Member (P.Embed IO) r, P.MemberWithError (P.Error Exception.IOException) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist Exception.IOException r k (Streamly.Array.Array Word.Word8)
persistAsByteArray keyToFilePath = Persist readBA writeBA deleteFile
 where
  readArrayFromHandle h = Streamly.fold Streamly.Array.write $ Streamly.unfold Streamly.Handle.read h
  writeArrayToHandle ba h = Streamly.fold (Streamly.Handle.write h) $ Streamly.unfold Streamly.Array.read ba
  readBA k = do
    let filePath = keyToFilePath k
    P.embed
      $        fmap Just (System.withFile filePath System.ReadMode readArrayFromHandle)
      `Exception.catch` (\(e::Exception.IOException) -> return Nothing)
  writeBA k ba = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    liftIO $ (System.withFile filePath System.WriteMode $ writeArrayToHandle ba) -- should we do on another thread??
  deleteFile k = liftIO $ System.removeFile (keyToFilePath k)
{-# INLINEABLE persistAsByteArray #-}


createDirIfNecessary
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => T.Text
  -> K.Sem r ()
createDirIfNecessary dir = K.wrapPrefix "createDirIfNecessary" $ do
  K.logLE K.Diagnostic $ "Checking if cache path (\"" <> dir <> "\") exists."
  existsE <-
    P.embed
    $         fmap Right (System.doesDirectoryExist (T.unpack dir))
    `Exception.catch` (\(e::Exception.IOException) -> return $ Left e)
  case existsE of
    Left e -> do
      K.logLE K.Diagnostic $ "\"" <> dir <> "\" exists."
      return ()
    Right exists -> case exists of
      False -> do
        K.logLE K.Info
          $  "Cache directory (\""
          <> dir
          <> "\") not found. Atttempting to create."
        P.embed
          $ System.createDirectoryIfMissing True (T.unpack dir)
      True -> return ()
{-# INLINEABLE createDirIfNecessary #-}



{-    


atomicMemLookup :: (Ord k, P.Member (P.Embed IO) r, P.MemberWithError (P.Error e) r)
              => AtomicMemCache e k ct
              -> k
              -> P.Sem r (Either (C.TMVar ct) ct) -- we either return the value or a tvar to be filled
atomicMemLookup cache key = P.embed $ C.atomically $ do    
  m <- C.readTVar cache    
  case M.lookup key m of    
    Just tmv -> fmap Right $ C.readTMVar tmv -- block until this has a value and then return it
    Nothing -> do
      tmv <- C.newEmptyTMVar -- create a new empty TMVar and put it in the map
      C.modifyTVar' cache (M.insert key tmv)
      return $ Left tmv           
{-# INLINEABLE atomicLookupC #-}

atomicReadC :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
            => Persist e r k ct
            -> PCache e k ct
            -> k
            -> P.Sem r (Either e ct)
atomicReadC (Persist readP _ _) cache key = do
  let cacheMsg t = K.logLE K.Diagnostic $ "cached asset at key=" <> (T.pack $ show key) <> " " <> t
  lookupResult <- atomicLookupC cache key 
  case lookupResult of
    Right x -> do
      case x of
        Right _ -> cacheMsg "exists in memory/found in persistent store."
        Left _ -> cacheMsg "not found in persistent store (on another thread)."
      return x
    Left tmv -> do
      cacheMsg "not in memory. Checking persistent store."
      readResult <- readP key
      case readResult of
        Left _ -> do
          cacheMsg "not found in persistent store (on this thread)."
          P.embed $ C.atomically $ do
            C.modifyTVar' cache (M.delete key) -- we delete the key so subsequent writes may succeed            
            C.putTMVar tmv readResult
        Right _ -> do
          cacheMsg "found in persistent store."
          P.embed $ C.atomically $ C.putTMVar tmv readResult
      return readResult
{-# INLINEABLE atomicReadC #-}

atomicWriteC
 :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => Persist e r k ct
  -> PCache e k ct
  -> k
  -> ct
  -> P.Sem r (Either e ())
atomicWriteC (Persist _ writeP _) cache key ctVal = do
  let cacheMsg t = K.logLE K.Diagnostic $ "cached asset at key=" <> (T.pack $ show key) <> " " <> t
  lookupResult <- atomicLookupC cache key
  case lookupResult of
    Right alreadyPresent -> do
      case alreadyPresent of
        Left _ -> cacheMsg "failed to load (on another thread)."
        Right _ -> cacheMsg "already in cache, ignoring this write attempt."
      return $ fmap (const ()) $ alreadyPresent
    Left tmv -> do
      cacheMsg "absent from cache.  Storing asset"
      P.embed $ C.atomically $ C.putTMVar tmv $ Right ctVal
      writeP key ctVal
{-# INLINEABLE atomicWriteC #-}

atomicDeleteC
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => Persist e r k ct
  -> PCache e k ct
  -> k
  -> P.Sem r (Either e ())
atomicDeleteC  (Persist _ _ deleteP) cache key = do
  let cacheMsg t = K.logLE K.Diagnostic $ "cached asset at key=" <> (T.pack $ show key) <> " " <> t
  deleted <- P.embed $ C.atomically $ do
    m <- C.readTVar cache
    case M.lookup key m of
      Nothing -> return False
      Just _ -> do
        C.modifyTVar' cache (M.delete key)
        return True 
  case deleted of
    True ->  cacheMsg "deleted from in-memory cache. Deleting from persistent store..." >> deleteP key
    False -> cacheMsg "not found though delete was called." >> return (Right ())
{-# INLINEABLE atomicDeleteC #-}

runPersistentAtomicCacheC :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
                          => Persist e r k ct -> PCache e k ct -> P.Sem (AtomicCache e k ct ': r) a -> P.Sem r a
runPersistentAtomicCacheC persist cache = 
  P.interpret $ \case
    AtomicRetrieve key -> atomicReadC persist cache key
    AtomicUpdate key mb -> case mb of
      Nothing -> atomicDeleteC persist cache key
      Just ct  -> atomicWriteC persist cache key ct
{-# INLINEABLE runPersistentAtomicCacheC #-}

runPersistentAtomicCacheCFromEmpty :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
                                   => Persist e r k ct -> P.Sem (AtomicCache e k ct ': r) a -> P.Sem r a
                                   
runPersistentAtomicCacheCFromEmpty persist x = do
  cache <- P.embed $ C.newTVarIO M.empty
  runPersistentAtomicCacheC persist cache x
{-# INLINEABLE runPersistentAtomicCacheCFromEmpty #-}  


atomicRead
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => (k -> P.Sem r (Either e ct))
  -> k
  -> P.Sem (P.AtomicState (M.Map k (C.TMVar ct)) ': r) (Either e ct)
atomicRead readF k = K.wrapPrefix "AtomicCache.atomicRead" $ do
--  K.logLE K.Diagnostic $ "here. " -- k=" <> (T.pack $ show k)
  tvM <- P.atomicGets $ M.lookup k
--  K.logLE K.Diagnostic "here"
  case tvM of
    Just tv -> do
      K.logLE K.Diagnostic
        $ "cached asset at key="
        <> (T.pack $ show k)
        <> " exists in memory (or is already being loaded/made on another thread)."
      fmap Right $ P.embed $ C.atomically $ C.readTMVar tv -- it exists so someone has already retrieved it or is in the process.  Wait for it.
    Nothing -> do
      K.logLE K.Diagnostic
        $  "cached asset at key="
        <> (T.pack $ show k)
        <> " not in memory. Checking persistent store."
      tv <- P.embed $ C.atomically $ C.newEmptyTMVar
      P.atomicModify (M.insert k tv)
      readResult <- P.raise $ readF k
      case readResult of
        Left e -> do
          K.logLE K.Diagnostic
            $  "cached asset at key="
            <> (T.pack $ show k)
            <> " not in persistent store."
          P.atomicModify (M.delete k)
          return $ Left e
        Right b -> do
          K.logLE K.Diagnostic
            $ "cached asset at key="
            <> (T.pack $ show k)
            <> " found and loaded from persistent store and added to in-memory cache."
          P.embed $ C.atomically $ C.putTMVar tv b
          return $ Right b
{-# INLINEABLE atomicRead #-}

atomicWrite
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => (k -> ct -> P.Sem r (Either e ()))
  -> k
  -> ct
  -> P.Sem (P.AtomicState (M.Map k (C.TMVar ct)) ': r) (Either e ())
atomicWrite writeF k ct = K.wrapPrefix "AtomicCache.atomicWrite" $ do
  K.logLE K.Diagnostic
    $  "Writing asset to cache (memory and persistent store) at key="
    <> (T.pack $ show k)
  tv <- P.embed $ C.atomically $ C.newTMVar ct -- TODO: do we want conversion to happen outside the STM transaction? How?
  P.atomicModify' $ M.alter (const $ Just tv) k
  P.raise $ writeF k ct
{-# INLINEABLE atomicWrite #-}

atomicDelete
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => (k -> P.Sem r (Either e ()))
  -> k
  -> P.Sem (P.AtomicState (M.Map k (C.TMVar ct)) ': r) (Either e ())
atomicDelete deleteF k = do
  K.logLE K.Diagnostic
    $  "Deleting asset from cache (memory and persistent store) at key="
    <> (T.pack $ show k)
  P.atomicModify $ M.alter (const Nothing) k
  P.raise $ deleteF k
{-# INLINEABLE atomicDelete #-}

-- | Interpret AtomicDataCache effect via AtomicState and Persist
runPersistentAtomicCacheInAtomicState
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => Persist e r k ct
  -> P.Sem (AtomicCache e k ct ': r) a
  -> P.Sem (P.AtomicState (M.Map k (C.TMVar ct)) ': r) a
runPersistentAtomicCacheInAtomicState (Persist readP writeP deleteP) =
  P.reinterpret $ \case
    AtomicRetrieve k  -> atomicRead readP k
    AtomicUpdate k mb -> case mb of
      Nothing -> atomicDelete deleteP k
      Just ct  -> atomicWrite writeP k ct
{-# INLINEABLE runPersistentAtomicCacheInAtomicState #-}

runPersistentAtomicCache
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => Persist e r k ct
  -> P.Sem (AtomicCache e k ct ': r) a
  -> P.Sem r a
runPersistentAtomicCache p mx = do
  tv <- P.embed $ C.atomically $ C.newTVar M.empty
  P.runAtomicStateTVar tv $ runPersistentAtomicCacheInAtomicState p mx
{-# INLINEABLE runPersistentAtomicCache #-}
-- | Persist functions for disk-based persistence with a strict ByteString interface
{-
We should perhaps do the writing, and maybe some version of the reading, on a separate thread.
We could launch a thread for each write?
Issues:
a. If the main thread exits before the write is finished, it will kill the write, I think.
b. We lose access to the return value.

We can maybe solve both by waiting for that thread to exit and grabbing the return value?  But
how to thread the Async return through?

We could add a State ([P.Async (Either PandocError ())]) or some such and await on all of them at the end?
-}
persistAsStrictByteString
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist X.IOException r k BS.ByteString
persistAsStrictByteString keyToFilePath = Persist readBS writeBS deleteFile
 where
  readBS k =
    liftIO
      $         fmap Right (BS.readFile (keyToFilePath k))
      `X.catch` (return . Left)
  writeBS k !b = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    liftIO $ fmap Right (BS.writeFile filePath b) `X.catch` (return . Left) -- maybe we should do this in another thread?
  deleteFile k =
    liftIO
      $         fmap Right (System.removeFile (keyToFilePath k))
      `X.catch` (return . Left)
{-# INLINEABLE persistAsStrictByteString #-}

-- | Persist functions for disk-based persistence with a lazy ByteString interface on the serialization side
persistAsByteString
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist X.IOException r k BL.ByteString
persistAsByteString keyToFilePath = Persist readBL writeBL deleteFile
 where
  readBL k =
    liftIO
      $         fmap Right (BL.readFile (keyToFilePath k))
      `X.catch` (return . Left)
  writeBL k bs = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk:"
    K.logLE K.Diagnostic $ "Bytestring is " <> (T.pack $ show $ BL.length bs) <> " bytes."
    liftIO $ fmap Right (BL.writeFile filePath bs) `X.catch` (return . Left) -- maybe we should do this in another thread?
  deleteFile k =
    liftIO
      $         fmap Right (System.removeFile (keyToFilePath k))
      `X.catch` (return . Left)
{-# INLINEABLE persistAsByteString #-}
{-
type StreamlyBytes = Streamly.Serial Word.Word8

persistStreamly :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist X.IOException r k StreamlyBytes StreamlyBytes 
persistStreamly keyToFilePath = Persist readStream writeStream deleteFile id
-}


-}
