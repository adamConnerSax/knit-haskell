{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Knit.Report.Cache
Description : Combinators for using the AtomicCache effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module adds types, combinators and knit functions for using knit-haskell with the DataCache effect. 

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.
-}
module Knit.Report.Cache
  (
    -- * Combinators
    store
  , retrieve
  , retrieveOrMake
  , retrieveOrMakeTransformed
  , clear
    -- * Experimental
  , CachedAction
  , CachedRunnable
  , makeRunnable
  , cacheAction
  , useCached
  )
where

import qualified Knit.Report.EffectStack       as K

import qualified Knit.Effect.AtomicCache       as C
import qualified Knit.Effect.Logger            as K

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Data.Serialize                as S

import           Text.Pandoc.Error              ( PandocError
                                                  ( PandocSomeError
                                                  , PandocIOError
                                                  )
                                                )
import qualified System.IO.Error               as IE

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P


type KnitCache = C.AtomicCache IE.IOError T.Text BS.ByteString

-- | A holder for a value that might already exist or with (monadic) instructions for making it
-- NB: Use this machinery with caution!  You will end up tying the environment where you need
-- the @a@ with the environment needed to build it (the @es@). This can be more painful than
-- it's worth just to avoid de-serialization.
data CachedAction es a where
  Made :: a -> CachedAction es a
  RetrieveOrMake :: S.Serialize b => T.Text -> (forall r. P.Members es r => P.Sem r b) -> (b -> a) -> CachedAction es a

instance Functor (CachedAction es) where
  fmap = mapCachedAction

mapCachedAction :: (a -> b) -> CachedAction es a -> CachedAction es b
mapCachedAction f (Made a) = Made $ f a
mapCachedAction f (RetrieveOrMake k action toA) =
  RetrieveOrMake k action (f . toA)

-- | Quantify (?) the @Members es r@ constraint so we can pass CacheHolders to functions without
-- those functions needing to know what effects the CH was built with as long as they
-- are memmbers of the stack used to call @'useCached'@
data CachedRunnable r a where
  CachedRunnable :: P.Members es r => CachedAction es a -> CachedRunnable r a

instance Functor (CachedRunnable r) where
  fmap = mapCachedRunnable

-- | Wrap a @CachedAction@ in a @CachedRunnable@ which witnesses that @r@ contains @es@.  
makeRunnable :: P.Members es r => CachedAction es a -> CachedRunnable r a
makeRunnable = CachedRunnable

mapCachedRunnable :: (a -> b) -> CachedRunnable r a -> CachedRunnable r b
mapCachedRunnable f (CachedRunnable ca) = CachedRunnable (mapCachedAction f ca)

-- | Create a @CachedAction@ for some action returning @a@.
-- Inference on the action cannot determine the @es@ argument
-- so you will usually have to specify it.
cacheAction
  :: S.Serialize b
  => T.Text
  -> (forall r . P.Members es r => P.Sem r b)
  -> (b -> a)
  -> CachedAction es a
cacheAction = RetrieveOrMake

-- | Get an action from a @CachedRunnable@.  This may be:
-- 1. The stored result of previously running the action
-- 2. The (deserialized) result of retrieving from the in-memory or on-disk cache
-- 3. The result of running the action.
-- In the case of 3, the result will be cached in memory and on-disk.
-- NB: The @CachedRunnable@ constructor requires @r@ to have the effects required
-- to run the contained action.
useCached
  :: (P.Members '[KnitCache, P.Error PandocError] r, K.LogWithPrefixesLE r)
  => CachedRunnable r a
  -> P.Sem r a
useCached (CachedRunnable ca) = case ca of
  Made x                      -> return x
  RetrieveOrMake key action f -> f <$> retrieveOrMake key action


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

cerealStrict :: S.Serialize a => C.Serialize PandocError a BS.ByteString
cerealStrict = C.Serialize S.encode (mapLeft PandocSomeError . S.decode)

cerealLazy :: S.Serialize a => C.Serialize PandocError a BL.ByteString
cerealLazy = C.Serialize S.encodeLazy (mapLeft PandocSomeError . S.decodeLazy)

-- | Store an @a@ (serialized to a strict @ByteString@) at key k. Throw PandocIOError on IOError.
store
  :: ( P.Members '[KnitCache, P.Error PandocError] r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text
  -> a
  -> P.Sem r ()
store k a = K.wrapPrefix "knitStore" $ do
  K.logLE K.Diagnostic $ "Called with k=" <> k
  P.mapError ioErrorToPandocError $ C.store cerealStrict k a

-- | Retrieve an a from the store at key k. Throw if not found or IOError
retrieve
  :: (P.Members '[KnitCache, P.Error PandocError] r, S.Serialize a)
  => T.Text
  -> P.Sem r a
retrieve k = P.mapError ioErrorToPandocError $ C.retrieve cerealStrict k

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k. 
retrieveOrMake
  :: forall a r
   . ( P.Members '[KnitCache, P.Error PandocError] r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text
  -> P.Sem r a
  -> P.Sem r a
retrieveOrMake k toMake = K.wrapPrefix "knitRetrieveOrMake" $ do
  ma <- C.retrieveMaybe cerealStrict k
  case ma of
    Nothing -> do
      K.logLE K.Diagnostic $ k <> " not found in cache. Making..."
      a <- toMake
      store k a
      K.logLE K.Diagnostic $ "Asset Stored."
      return a
    Just a -> do
      K.logLE K.Diagnostic $ k <> " found in cache."
      return a

retrieveOrMakeTransformed
  :: forall a b r
   . ( P.Members '[KnitCache, P.Error PandocError] r
     , K.LogWithPrefixesLE r
     , S.Serialize b
     )
  => (a -> b)
  -> (b -> a)
  -> T.Text
  -> P.Sem r a
  -> P.Sem r a
retrieveOrMakeTransformed toSerializable fromSerializable k toMake =
  fmap fromSerializable $ retrieveOrMake k (fmap toSerializable toMake)

-- | Clear the @b@ stored at key k.
clear :: K.KnitEffects r => T.Text -> P.Sem r ()
clear k = P.mapError ioErrorToPandocError $ C.clear k


ioErrorToPandocError :: IE.IOError -> PandocError
ioErrorToPandocError e = PandocIOError (show e) e
