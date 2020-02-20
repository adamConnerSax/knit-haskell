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
  , lazyPersistAsByteString
  , strictPersistAsByteString
    -- * Interpretations
  , runPersistentAtomicCache
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import qualified Polysemy.AtomicState          as P
import qualified Knit.Effect.Logger            as K

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Control.Concurrent.STM        as C
import qualified Control.Exception             as X
import           Control.Monad                  ( join )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )

import qualified System.Directory              as S

-- | data type to store encode/decode functins for users serializer of choice
data Serialize e a b where
  Serialize :: (a -> b) -> (b -> Either e a) -> Serialize e a b

-- | This is a Key/Value store with a parameterized error type @e@ and
-- @Either e@ as its return type
data AtomicCache e k b m a where
  AtomicRetrieve :: k -> AtomicCache e k b m (Either e b)
  AtomicUpdate :: k -> Maybe b -> AtomicCache e k b m (Either e ())

P.makeSem ''AtomicCache

eitherThrow :: P.Member (P.Error e) r => P.Sem r (Either e a) -> P.Sem r a
eitherThrow x = do
  ea <- x
  case ea of
    Left  e -> P.throw e
    Right a -> return a --fmap (either (P.throw @e) id)

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

-- | Combinator to combine the action of serializing and caching
store
  :: P.Members '[AtomicCache e1 k b, P.Error e1] r
  => Serialize e2 a b
  -> k
  -> a
  -> P.Sem r ()
store (Serialize encode _) k x = eitherThrow $ atomicUpdate k (Just $ encode x)

-- | Combinator to combine the action of retrieving from cache and deserializing
-- NB. Either action may have an error
retrieve
  :: P.Members '[AtomicCache e1 k b, P.Error e1, P.Error e2] r
  => Serialize e2 a b
  -> k
  -> P.Sem r a
retrieve (Serialize _ decode) k =
  eitherThrow $ fmap decode $ eitherThrow $ atomicRetrieve k

retrieveMaybe
  :: forall e1 e2 k a b r
   . P.Members '[AtomicCache e1 k b] r
  => Serialize e2 a b
  -> k
  -> P.Sem r (Maybe a)
retrieveMaybe (Serialize _ decode) k =
  fmap (join . fmap (hush . decode) . hush) $ atomicRetrieve k

-- | Combinator for clearing the cache at a given key
clear :: P.Members '[AtomicCache e k b, P.Error e] r => k -> P.Sem r ()
clear k = eitherThrow $ atomicUpdate k Nothing

-- | Data type to hold the persistence functions of:
-- reading from the store
-- writing to the store
-- clearing the store
data Persist e r k b where
  Persist ::  (k -> P.Sem r (Either e b))
           -> (k -> b -> P.Sem r (Either e ()))
           -> (k -> P.Sem r (Either e ()))
           -> Persist e r k b

atomicRead
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => (k -> P.Sem r (Either e b))
  -> k
  -> P.Sem (P.AtomicState (M.Map k (C.TMVar b)) ': r) (Either e b)
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

atomicWrite
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => (k -> b -> P.Sem r (Either e ()))
  -> k
  -> b
  -> P.Sem (P.AtomicState (M.Map k (C.TMVar b)) ': r) (Either e ())
atomicWrite writeF k b = K.wrapPrefix "AtomicCache.atomicWrite" $ do
  K.logLE K.Diagnostic
    $  "Writing asset to cache (memory and persistent store) at key="
    <> (T.pack $ show k)
  tv <- P.embed $ C.atomically $ C.newTMVar b
  P.atomicModify $ M.alter (const $ Just tv) k
  P.raise $ writeF k b

atomicDelete
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => (k -> P.Sem r (Either e ()))
  -> k
  -> P.Sem (P.AtomicState (M.Map k (C.TMVar b)) ': r) (Either e ())
atomicDelete deleteF k = do
  K.logLE K.Diagnostic
    $  "Deleting asset from cache (memory and persistent store) at key="
    <> (T.pack $ show k)
  P.atomicModify $ M.alter (const Nothing) k
  P.raise $ deleteF k


-- | Interpret AtomicDataCache effect via AtomicState and Persist
runPersistentAtomicCacheInAtomicState
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => Persist e r k b
  -> P.Sem (AtomicCache e k b ': r) a
  -> P.Sem (P.AtomicState (M.Map k (C.TMVar b)) ': r) a
runPersistentAtomicCacheInAtomicState (Persist readP writeP deleteP) =
  P.reinterpret $ \case
    AtomicRetrieve k  -> atomicRead readP k
    AtomicUpdate k mb -> case mb of
      Nothing -> atomicDelete deleteP k
      Just b  -> atomicWrite writeP k b

runPersistentAtomicCache
  :: (Ord k, Show k, P.Member (P.Embed IO) r, K.LogWithPrefixesLE r)
  => Persist e r k b
  -> P.Sem (AtomicCache e k b ': r) a
  -> P.Sem r a
runPersistentAtomicCache p mx = do
  tv <- P.embed $ C.atomically $ C.newTVar M.empty
  P.runAtomicStateTVar tv $ runPersistentAtomicCacheInAtomicState p mx

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
strictPersistAsByteString
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist X.IOException r k BS.ByteString
strictPersistAsByteString keyToFilePath = Persist readBS writeBS deleteFile
 where
  readBS k =
    liftIO
      $         fmap Right (BS.readFile (keyToFilePath k))
      `X.catch` (return . Left)
  writeBS k b = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    liftIO $ fmap Right (BS.writeFile filePath b) `X.catch` (return . Left) -- maybe we should do this in another thread?
  deleteFile k =
    liftIO
      $         fmap Right (S.removeFile (keyToFilePath k))
      `X.catch` (return . Left)

-- | Persist functions for disk-based persistence with a lazy ByteString interface
lazyPersistAsByteString
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => (k -> FilePath)
  -> Persist X.IOException r k BL.ByteString
lazyPersistAsByteString keyToFilePath = Persist readBS writeBS deleteFile
 where
  readBS k =
    liftIO
      $         fmap Right (BL.readFile (keyToFilePath k))
      `X.catch` (return . Left)
  writeBS k b = do
    let filePath     = (keyToFilePath k)
        (dirPath, _) = T.breakOnEnd "/" (T.pack filePath)
    _ <- createDirIfNecessary dirPath
    K.logLE K.Diagnostic $ "Writing serialization to disk."
    liftIO $ fmap Right (BL.writeFile filePath b) `X.catch` (return . Left) -- maybe we should do this in another thread?
  deleteFile k =
    liftIO
      $         fmap Right (S.removeFile (keyToFilePath k))
      `X.catch` (return . Left)

createDirIfNecessary
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r)
  => T.Text
  -> K.Sem r (Either X.IOException ())
createDirIfNecessary dir = K.wrapPrefix "createDirIfNecessary" $ do
  K.logLE K.Diagnostic $ "Checking if cache path (\"" <> dir <> "\") exists."
  existsE <-
    P.embed
    $         fmap Right (S.doesDirectoryExist (T.unpack dir))
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
          $         fmap Right (S.createDirectoryIfMissing True (T.unpack dir))
          `X.catch` (return . Left)
      True -> return $ Right ()

