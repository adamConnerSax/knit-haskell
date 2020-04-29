{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
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
    AtomicCache
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
import qualified Control.Exception             as X
--import           Control.Monad                  ( join )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )

import qualified Streamly                      as Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Memory.Array         as Streamly.Array
import qualified Streamly.FileSystem.Handle    as Streamly.Handle

import qualified System.IO                     as System
import qualified System.Directory              as System

-- | data type to store encode/decode functins for users serializer of choice
-- | We encode (serialize) to @enc@ and decode (deserialize) from @dec@
data Serialize r e a ct where
  Serialize :: (a -> P.Sem r ct) -> (ct -> P.Sem r (Either e a)) -> Serialize r e a ct

-- | This is a Key/Value store with a parameterized error type @e@ and
-- @Either e@ as its return type
data AtomicCache e k ct m a where
  AtomicRetrieve :: k -> AtomicCache e k ct m (Either e ct)
  AtomicUpdate :: k -> Maybe ct -> AtomicCache e k ct m (Either e ())

P.makeSem ''AtomicCache

eitherThrow :: P.Member (P.Error e) r => P.Sem r (Either e a) -> P.Sem r a
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
  :: (Show k, P.Members '[AtomicCache e1 k ct, P.Error e1] r,  K.LogWithPrefixesLE r)
  => Serialize r e2 a ct
  -> k
  -> a
  -> P.Sem r ()
store (Serialize encode _) k x = K.wrapPrefix "Knit.Atomic.Cache.store" $ do
  K.logLE K.Diagnostic $ "encoding (serializing) data for key=" <> (T.pack $ show k) 
  encoded <- encode x
  K.logLE K.Diagnostic $ "Storing encoded data in cache for key=" <> (T.pack $ show k) 
  eitherThrow $ atomicUpdate k (Just encoded)
{-# INLINEABLE store #-}

-- | Combinator to combine the action of retrieving from cache and deserializing
-- NB. Either action may have an error
retrieve
  :: P.Members '[AtomicCache e1 k ct, P.Error e1, P.Error e2] r
  => Serialize r e2 a ct
  -> k
  -> P.Sem r a
retrieve (Serialize _ decode) k = eitherThrow (atomicRetrieve k) >>= eitherThrow . decode 
{-# INLINEABLE retrieve #-}

retrieveMaybe
  :: forall e1 e2 k a ct r
   . P.Members '[AtomicCache e1 k ct] r
  => Serialize r e2 a ct
  -> k
  -> P.Sem r (Maybe a)
retrieveMaybe (Serialize _ decode) k = fmap hush (atomicRetrieve k) >>= fmap (>>= hush) . traverse decode 
{-# INLINEABLE retrieveMaybe #-}

-- | Combinator for clearing the cache at a given key
clear :: P.Members '[AtomicCache e k ct, P.Error e] r => k -> P.Sem r ()
clear k = eitherThrow $ atomicUpdate k Nothing
{-# INLINEABLE clear #-}

-- | Data type to hold the persistence functions of:
-- reading from the store
-- writing to the store
-- clearing the store
data Persist e r k ct where
  Persist :: (k -> P.Sem r (Either e ct))
          -> (k -> ct -> P.Sem r (Either e ()))
          -> (k -> P.Sem r (Either e ()))
          -> Persist e r k ct

-- structure for actual cache
-- outer TVar so only one thread can get the inner TMVar at a time
-- TMVar so we can block if mulitple threads are trying to read or update
type Cache k v = C.TVar (M.Map k (C.TMVar v))

-- Either so we can fail to read
type PCache e k v = Cache k (Either e v)

-- read interpret via Cache and Persistence layer
atomicLookupC :: (Ord k,  P.Member (P.Embed IO) r)
              => PCache e k ct
              -> k
              -> P.Sem r (Either (C.TMVar (Either e ct)) (Either e ct))
atomicLookupC cache key = P.embed $ C.atomically $ do    
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
        Left _ -> cacheMsg "not found in persistent store (on this thread)."
        Right _ -> cacheMsg "found in persistent store."
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

persistAsByteStreamly
  :: (P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist X.IOException r k (Streamly.SerialT Identity Word.Word8)
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
    P.embed
      $         fmap Right (System.withFile filePath System.ReadMode readFromHandle)
      `X.catch` (return . Left)
  writeBA k bs = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    liftIO $ fmap Right (System.withFile filePath System.WriteMode $ writeToHandle bs) `X.catch` (return . Left) -- maybe we should do this in another thread?
  deleteFile k =
    liftIO
      $         fmap Right (System.removeFile (keyToFilePath k))
      `X.catch` (return . Left)      
{-# INLINEABLE persistAsByteStreamly #-}


persistAsByteArray
  :: (P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist X.IOException r k (Streamly.Array.Array Word.Word8)
persistAsByteArray keyToFilePath = Persist readBA writeBA deleteFile
 where
  readArrayFromHandle h = Streamly.fold Streamly.Array.write $ Streamly.unfold Streamly.Handle.read h
  writeArrayToHandle ba h = Streamly.fold (Streamly.Handle.write h) $ Streamly.unfold Streamly.Array.read ba
  readBA k = do
    let filePath = keyToFilePath k
    P.embed
      $         fmap Right (System.withFile filePath System.ReadMode readArrayFromHandle)
      `X.catch` (return . Left)
  writeBA k ba = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    liftIO $ fmap Right (System.withFile filePath System.WriteMode $ writeArrayToHandle ba) `X.catch` (return . Left) -- maybe we should do this in another thread?
  deleteFile k =
    liftIO
      $         fmap Right (System.removeFile (keyToFilePath k))
      `X.catch` (return . Left)      
{-# INLINEABLE persistAsByteArray #-}

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

createDirIfNecessary
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => T.Text
  -> K.Sem r (Either X.IOException ())
createDirIfNecessary dir = K.wrapPrefix "createDirIfNecessary" $ do
  K.logLE K.Diagnostic $ "Checking if cache path (\"" <> dir <> "\") exists."
  existsE <-
    P.embed
    $         fmap Right (System.doesDirectoryExist (T.unpack dir))
    `X.catch` (return . Left)
  case existsE of
    Left e -> do
      K.logLE K.Diagnostic $ "\"" <> dir <> "\" exists."
      return $ Left e
    Right exists -> case exists of
      False -> do
        K.logLE K.Info
          $  "Cache directory (\""
          <> dir
          <> "\") not found. Atttempting to create."
        P.embed
          $         fmap Right (System.createDirectoryIfMissing True (T.unpack dir))
          `X.catch` (return . Left)
      True -> return $ Right ()
{-# INLINEABLE createDirIfNecessary #-}
