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
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Knit.Report.Cache
Description : Combinators for using the AtomicCache effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module adds types, combinators and knit functions for using knit-haskell with the AtomicCache effect. 

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.
-}
module Knit.Report.Cache
  (
    -- * Direct Combinators
    store
  , retrieve
  , retrieveOrMake
  , retrieveOrMakeTransformed
  , clear
  , asCached

    -- * "Lazy" interface
  , Cached
  , cacheValue
  , cacheRetrieval
  , cacheAction
  , cacheTransformedAction
  , useCached
  , forceCached
  )
where

import qualified Knit.Report.EffectStack       as K

import qualified Knit.Effect.AtomicCache       as C
import qualified Knit.Effect.Logger            as K
import qualified Knit.Effect.PandocMonad       as K
                                                ( textToPandocText )

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

-- | A holder for a value that might already exist or with a key to retrieve it
-- or with (monadic) instructions for making it.  This allows cached things to
-- be passed "lazily", that is, not retrieved or computed unless needed.
-- NB: Use this machinery with caution!  You will end up tying the environment where you need
-- the @a@ with the environment needed to build it (the @es@). This can be more painful than
-- it's worth just to avoid de-serialization.
data Cached es a where
  Unwrap :: a -> Cached '[] a -- ^ We already have a here. Using is just unwrapping.
  Retrieve :: S.Serialize b => (b -> a) -> T.Text -> Cached '[] a -- ^ We need to retrieve and convert.  Failure to find is an error.
  RetrieveOrMake :: S.Serialize b -- ^ We may be able to retrieve. If not we can make by running the given action.
                 => (b -> a)
                 -> T.Text
                 -> (forall r. P.Members es r => P.Sem r b)
                 -> Cached es a

instance Functor (Cached es) where
  fmap = mapCached

mapCached :: (a -> b) -> Cached es a -> Cached es b
mapCached f (Unwrap a                   ) = Unwrap $ f a
mapCached f (Retrieve toA k             ) = Retrieve (f . toA) k
mapCached f (RetrieveOrMake toA k action) = RetrieveOrMake (f . toA) k action

-- | Create a @Cached a@ for some action returning @b@
-- and given a function @b -> a@.
-- Inference on the action cannot determine the @es@ argument
-- so you will usually have to specify it.
cacheAction
  :: S.Serialize b
  => (b -> a)
  -> T.Text
  -> (forall r . P.Members es r => P.Sem r b)
  -> Cached es a
cacheAction = RetrieveOrMake

-- | Create a @Cached@
cacheTransformedAction
  :: S.Serialize b
  => (a -> b)
  -> (b -> a)
  -> T.Text
  -> (forall r . P.Members es r => P.Sem r a)
  -> Cached es a
cacheTransformedAction toB toA k action = cacheAction toA k (toB <$> action)

-- | Cache an already computed value.
-- Stores serialized data (and returns the value as a "Cached"
-- for passing to functions which expect one)
cacheValue
  :: ( S.Serialize b
     , P.Members '[KnitCache, P.Error PandocError] r
     , P.Members K.PrefixedLogEffectsLE r
     )
  => (a -> b)
  -> T.Text
  -> a
  -> P.Sem r (Cached '[] a)
cacheValue toB k a = store k (toB a) >> return (Unwrap a)

-- | Wrap an existing value in Cached.  Sometimes useful when
-- a function requires a Cached and you just have the value.
asCached :: a -> Cached '[] a
asCached = Unwrap

-- | Wrap a retrieval as a Cached for passing to functions which expect one.
-- Retrieving doesn't happen until @useAction@ is called.
cacheRetrieval :: S.Serialize b => (b -> a) -> T.Text -> Cached '[] a
cacheRetrieval = Retrieve


-- | Use a Cached.  Will trigger retrieval and/or running the action
-- if required.
useCached
  :: ( P.Members '[KnitCache, P.Error PandocError] r
     , P.Members K.PrefixedLogEffectsLE r
     , P.Members es r
     )
  => Cached es a
  -> P.Sem r a
useCached (Unwrap a                   ) = return a
useCached (Retrieve toA k             ) = toA <$> retrieve k
useCached (RetrieveOrMake toA k action) = toA <$> retrieveOrMake k action

-- | force the value to be made or retrieved,
-- returning a new @Cached@ which reflects that state.
forceCached
  :: ( P.Members '[KnitCache, P.Error PandocError] r
     , P.Members K.PrefixedLogEffectsLE r
     , P.Members es r
     )
  => Cached es a
  -> P.Sem r (Cached '[] a)
forceCached x = Unwrap <$> useCached x

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

cerealStrict :: S.Serialize a => C.Serialize PandocError a BS.ByteString
cerealStrict = C.Serialize
  S.encode
  (mapLeft (PandocSomeError . K.textToPandocText . T.pack) . S.decode)

cerealLazy :: S.Serialize a => C.Serialize PandocError a BL.ByteString
cerealLazy = C.Serialize
  S.encodeLazy
  (mapLeft (PandocSomeError . K.textToPandocText . T.pack) . S.decodeLazy)

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
ioErrorToPandocError e = PandocIOError (K.textToPandocText $ T.pack $ show e) e
