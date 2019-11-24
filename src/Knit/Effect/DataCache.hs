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
Module      : Knit.Effect.DataCache
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
module Knit.Effect.DataCache
  (
    -- * Effect
    DataCache(..)
    -- * Actions
  , retrieveCached
  , updateCached
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
  , runPersistentCache
  , runMapCache
  , runPersistentMapCache
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import qualified Polysemy.State                as P

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Control.Exception             as C
import           Control.Monad                  ( join )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )

import qualified System.Directory              as S

-- | data type to store encode/decode functins for users serializer of choice
data Serialize e a b where
  Serialize :: (a -> b) -> (b -> Either e a) -> Serialize e a b

-- | This is a Key/Value store with a parameterized error type @e@ and
-- @Either e@ as its return type
data DataCache e k b m a where
  RetrieveCached :: k -> DataCache e k b m (Either e b)
  UpdateCached :: k -> Maybe b -> DataCache e k b m (Either e ())

P.makeSem ''DataCache

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
  :: P.Members '[DataCache e k b, P.Error e] r
  => Serialize e a b
  -> k
  -> a
  -> P.Sem r ()
store (Serialize encode _) k x = eitherThrow $ updateCached k (Just $ encode x)

-- | Combinator to combine the action of retrieving from cache and deserializing
-- NB. Either action may have an error
retrieve
  :: P.Members '[DataCache e1 k b, P.Error e1, P.Error e2] r
  => Serialize e2 a b
  -> k
  -> P.Sem r a
retrieve (Serialize _ decode) k =
  eitherThrow $ fmap decode $ eitherThrow $ retrieveCached k

retrieveMaybe
  :: forall e1 e2 k a b r
   . P.Members '[DataCache e1 k b] r
  => Serialize e2 a b
  -> k
  -> P.Sem r (Maybe a)
retrieveMaybe (Serialize _ decode) k =
  fmap (join . fmap (hush . decode) . hush) $ retrieveCached @e1 k


-- | Combinator for clearing the cache at a given key
clear :: P.Members '[DataCache e k b, P.Error e] r => k -> P.Sem r ()
clear k = eitherThrow $ updateCached k Nothing

-- | Data type to hold the persistence functions of:
-- reading from the store
-- writing to the store
-- clearing the store
data Persist e r k b where
  Persist ::  (k -> P.Sem r (Either e b))
           -> (k -> b -> P.Sem r (Either e ()))
           -> (k -> P.Sem r (Either e ()))
           -> Persist e r k b

-- | Interpret DataCache effect via Persist
runPersistentCache
  :: Persist e r k b -> P.Sem (DataCache e k b ': r) a -> P.Sem r a
runPersistentCache (Persist readP writeP deleteP) = P.interpret $ \case
  RetrieveCached k  -> readP k
  UpdateCached k mb -> case mb of
    Nothing -> deleteP k
    Just b  -> writeP k b

-- | Interpret DataCache effect via Map, using a 'State' effect
runCacheAsMapState
  :: (Ord k, Show k)
  => (T.Text -> e)
  -> P.Sem (DataCache e k b ': r) a
  -> P.Sem (P.State (M.Map k b) ': r) a
runCacheAsMapState toError = P.reinterpret $ \case
  RetrieveCached k ->
    maybe
        (  Left
        $  toError
        $  "Couldn't find \""
        <> (T.pack $ show k)
        <> "\" in cache"
        )
        Right
      <$> (P.gets $ M.lookup k)
  UpdateCached k mb -> Right <$> (P.modify $ M.alter (const mb) k)

-- | Interpret DataCache effect via Map
runMapCache
  :: (Ord k, Show k)
  => (T.Text -> e)
  -> P.Sem (DataCache e k b ': r) a
  -> P.Sem r a
runMapCache toError =
  fmap snd . P.runState M.empty . runCacheAsMapState toError

-- | Interpret DataCache effect via Map and Persist, using a 'State' effect
runPersistentMapStateCache
  :: Ord k
  => Persist e r k b
  -> P.Sem (DataCache e k b ': r) a
  -> P.Sem (P.State (M.Map k b) ': r) a
runPersistentMapStateCache (Persist readP writeP deleteP) =
  P.reinterpret $ \case
    RetrieveCached k -> do
      inMap <- P.gets $ M.lookup k
      case inMap of
        Just b  -> return $ Right b
        Nothing -> P.raise $ readP k
    UpdateCached k mb -> do
      P.modify $ M.alter (const mb) k
      case mb of
        Nothing -> P.raise $ deleteP k
        Just b  -> P.raise $ writeP k b

-- | Interpret DataCache effect via Map and Persist.
-- | Map is checked first on retrieve.
runPersistentMapCache
  :: Ord k => Persist e r k b -> P.Sem (DataCache e k b ': r) a -> P.Sem r a
runPersistentMapCache p =
  fmap snd . P.runState M.empty . runPersistentMapStateCache p

-- | Persist functions for disk-based persistence with a strict ByteString interface
strictPersistAsByteString
  :: P.Members '[P.Embed IO] r
  => (k -> FilePath)
  -> Persist C.IOException r k BS.ByteString
strictPersistAsByteString keyToFilePath = Persist readBS writeBS deleteFile
 where
  readBS k =
    liftIO
      $         fmap Right (BS.readFile (keyToFilePath k))
      `C.catch` (return . Left)
  writeBS k b =
    liftIO
      $         fmap Right (BS.writeFile (keyToFilePath k) b)
      `C.catch` (return . Left)
  deleteFile k =
    liftIO
      $         fmap Right (S.removeFile (keyToFilePath k))
      `C.catch` (return . Left)

-- | Persist functions for disk-based persistence with a lazy ByteString interface
lazyPersistAsByteString
  :: P.Members '[P.Embed IO, P.Error C.IOException] r
  => (k -> FilePath)
  -> Persist C.IOException r k BL.ByteString
lazyPersistAsByteString keyToFilePath = Persist readBS writeBS deleteFile
 where
  readBS k =
    liftIO
      $         fmap Right (BL.readFile (keyToFilePath k))
      `C.catch` (return . Left)
  writeBS k b =
    liftIO
      $         fmap Right (BL.writeFile (keyToFilePath k) b)
      `C.catch` (return . Left)
  deleteFile k =
    liftIO
      $         fmap Right (S.removeFile (keyToFilePath k))
      `C.catch` (return . Left)


{-
data OnDemand r a where
  UnMade :: P.Sem r a -> OnDemand r a
  Made :: a -> OnDemand r a

data OnDemandCache k b m a where
  Retrieve :: k -> OnDemandCache k b m (Maybe b)


make :: OnDemand r a -> P.Sem r (OnDemand r a)
make (UnMade ma) = do
  a <- ma
  return (Made a)
make (Made a) = Made a
-}
