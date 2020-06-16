{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
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
    -- * Types
    KnitCache
    -- * Cache Combinators
  , store
  , retrieve
  , retrieveOrMake
  , retrieveOrMakeTransformed
  , storeStream
  , retrieveStream
  , retrieveOrMakeStream
  , retrieveOrMakeTransformedStream
  , clear
  , clearIfPresent
  )
where

--import qualified Knit.Report.EffectStack       as K

import qualified Knit.Effect.AtomicCache       as C
import           Knit.Effect.AtomicCache        (clear, clearIfPresent)
import qualified Knit.Effect.Logger            as K

import           Control.Monad (join)
import qualified Control.Monad.Catch.Pure      as Exceptions
--import qualified Control.Monad.State.Strict    as State

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor.Identity          (Identity(..))
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import qualified Data.Serialize                as S
import qualified Data.Word                     as Word

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import qualified Polysemy.State                as P

import qualified System.Directory              as System

import qualified Streamly                      as Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Internal.Prelude              as Streamly
import qualified Streamly.Data.Fold                 as Streamly.Fold
import qualified Streamly.Internal.Data.Fold                 as Streamly.Fold
import qualified Streamly.Memory.Array         as Streamly.Array
import qualified Streamly.Internal.Memory.Array         as Streamly.Array
import qualified Streamly.Internal.Data.Array           as Streamly.Data.Array
import qualified Streamly.External.Cereal      as Streamly.Cereal
import qualified Streamly.External.ByteString as Streamly.ByteString



type CacheData = Streamly.Array.Array Word.Word8

type KnitCache = C.Cache T.Text CacheData


-- | serialize a Serializable structure to the CacheData type.
knitSerialize :: ( S.Serialize a
                 , P.Member (P.Embed IO) r
                 , P.MemberWithError (P.Error C.CacheError) r
                 )
              => C.Serialize r a CacheData
knitSerialize = cerealArray
{-# INLINEABLE knitSerialize #-}

-- | serialize a Streamly stream of Serializable structures to the CacheData type.
knitSerializeStream :: (S.Serialize a
                       , P.Member (P.Embed IO) r                
                       , P.MemberWithError (P.Error Exceptions.SomeException) r
                       , P.MemberWithError (P.Error C.CacheError) r
                       )
                       => C.Serialize r (Streamly.SerialT (K.Sem r) a) CacheData
knitSerializeStream = cerealStreamViaListArray
{-# INLINEABLE knitSerializeStream #-}

-- | Map the left side of an Either
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right
{-# INLINEABLE mapLeft #-}

-- | Encode/Decode functions for serializing to strict ByteStrings
cerealStrict :: (S.Serialize a, P.MemberWithError (P.Error C.CacheError) r)
             => C.Serialize r a BS.ByteString
cerealStrict = C.Serialize 
  (\a -> return $ (S.encode a, a))
  (P.fromEither @C.CacheError . mapLeft (C.DeSerializationError . T.pack) . S.decode)
  (fromIntegral . BS.length)
{-# INLINEABLE cerealStrict #-}

-- | Encode/Decode functions for serializing to lazy ByteStrings
cereal :: (S.Serialize a, P.MemberWithError (P.Error C.CacheError) r)
       => C.Serialize r a BL.ByteString
cereal = C.Serialize
  (\a -> return $ (S.encodeLazy a, a))
  (P.fromEither . mapLeft (C.DeSerializationError . T.pack) . S.decodeLazy)
  BL.length
{-# INLINEABLE cereal #-}

-- | Encode/Decode functions for serializing to Streamly Streams
cerealStreamly :: (S.Serialize a
                  , P.Member (P.Embed IO) r
                  , P.MemberWithError (P.Error C.CacheError) r
                  ) => C.Serialize r a (Streamly.SerialT Identity Word.Word8)
cerealStreamly = C.Serialize
  (\a -> return $  (Streamly.Cereal.encodeStreamly a, a))
  (P.fromEither . mapLeft C.DeSerializationError . runIdentity . Streamly.Cereal.decodeStreamly)
  (fromIntegral . runIdentity . Streamly.length)
{-# INLINEABLE cerealStreamly #-}

{-
cerealStream :: (S.Serialize a
                , P.Member (P.Embed IO) r                
                , P.MemberWithError (P.Error Exceptions.SomeException) r
                , P.MemberWithError (P.Error C.CacheError) r
                )
             => C.Serialize r (Streamly.SerialT (P.Sem r) a) (Streamly.SerialT Identity Word.Word8)
cerealStream = C.Serialize
  (\a -> fmap Streamly.fromList . Streamly.toList . Streamly.Cereal.encodeStream)
  (\x -> Polysemy.MonadCatch.absorbMonadCatch $ return $ Streamly.Cereal.decodeStream $ Streamly.generally x)
  (fromIntegral . runIdentity . Streamly.length)
{-# INLINEABLE cerealStream #-}
-}

-- | Encode/Decode functions for serializing to Streamly Arrays
cerealArray :: (S.Serialize a
               , P.Member (P.Embed IO) r
               , P.MemberWithError (P.Error C.CacheError) r
               ) => C.Serialize r a (Streamly.Array.Array Word.Word8)
cerealArray = C.Serialize
  (\a -> return $ (Streamly.Cereal.encodeStreamlyArray a, a))
  (P.fromEither . mapLeft C.DeSerializationError . Streamly.Cereal.decodeStreamlyArray)
  (fromIntegral . Streamly.Array.length)
{-# INLINEABLE cerealArray #-}

-- | Encode/Decode functions for serializing Streamly streams to Streamly Arrays
-- When encoding, we also return a "buffered" stream so that the input stream is only "run" once.
cerealStreamArray :: forall r a.(S.Serialize a                                
                     , P.Member (P.Embed IO) r                
                     , P.MemberWithError (P.Error Exceptions.SomeException) r
                     , P.MemberWithError (P.Error C.CacheError) r
                     )
                  => C.Serialize r (Streamly.SerialT (P.Sem r) a) (Streamly.Array.Array Word.Word8)
cerealStreamArray = C.Serialize
  (Streamly.fold (Streamly.Fold.tee
                 (Streamly.Fold.lmapM (Streamly.Array.fromStream . Streamly.Cereal.encodeStreamly) $ Streamly.Fold.mconcat)
                 (fmap (Streamly.Data.Array.toStream) Streamly.Data.Array.write)
                 )
  )
  (return . fixMonadCatch . Streamly.Cereal.decodeStreamArray)
  (fromIntegral . Streamly.Array.length)
{-# INLINEABLE cerealStreamArray #-}

-- | Encode/Decode functions for serializing Streamly streams to Streamly Arrays, using the Cereal functions to encode/decode lists.
-- When encoding, we also return a "buffered" stream so that the input stream is only "run" once.
cerealStreamViaListArray :: (S.Serialize a
                            , P.Member (P.Embed IO) r                
                            , P.MemberWithError (P.Error Exceptions.SomeException) r
                            , P.MemberWithError (P.Error C.CacheError) r
                            )
                         => C.Serialize r (Streamly.SerialT (P.Sem r) a) (Streamly.Array.Array Word.Word8)
cerealStreamViaListArray = C.Serialize
  (Streamly.fold (Streamly.Fold.tee
                   (fmap ((Streamly.ByteString.toArray . S.runPut . S.putListOf S.put) ) $ Streamly.Fold.toList)
                   (fmap (Streamly.Data.Array.toStream) Streamly.Data.Array.write)
                 )
  )
  (P.fromEither . mapLeft (C.DeSerializationError . T.pack) . S.runGet (Streamly.Cereal.getStreamOf S.get) . Streamly.ByteString.fromArray)
  (fromIntegral . Streamly.Array.length)
{-# INLINEABLE cerealStreamViaListArray #-}

-- | Store an @a@ (serialized) at key k. Throw PandocIOError on IOError.
store
  :: ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text
  -> a
  -> P.Sem r ()
store k a = K.wrapPrefix ("Knit.store (key=" <> k <> ")") $ do
  K.logLE K.Diagnostic $ "Called with k=" <> k 
  C.encodeAndStore knitSerialize k a
{-# INLINEABLE store #-}

-- | Retrieve an a from the store at key k. Throw if not found or IOError.
-- assumes no issues with expiry time.
retrieve
  :: (P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     ,  K.LogWithPrefixesLE r
     , S.Serialize a)
  => T.Text
  -> P.Sem r a
retrieve k =  K.wrapPrefix ("Cache.retrieve (key=" <> k <> ")")
              $ fmap C.unWithCacheTime $ C.retrieveAndDecode knitSerialize Nothing k
{-# INLINEABLE retrieve #-}

--type NewestDepS = State.State (Maybe Time.UTCTime)
--type WithNewestDep r a = NewestDepS (P.Sem r a)

type NewestDepEff = P.State (Maybe Time.UTCTime)
type WithNewestDep r = P.Sem (NewestDepEff ': r) 

updateNewestTime :: Time.UTCTime -> WithNewestDep r ()
updateNewestTime t = P.modify f where
  f newestDepM = case newestDepM of
    Nothing -> Just t
    Just t' -> Just $ max t t'
    --(maybe (Just t) (max t)) 

data WithExpiryAction g a where
  WithExpiryAction :: Maybe Time.UTCTime -> g a -> WithExpiryAction g a
--  deriving (Functor)

mapExpiryAction :: (g a -> h b) -> WithExpiryAction g a -> WithExpiryAction h b
mapExpiryAction f (WithExpiryAction t ga) = WithExpiryAction t (f ga)

fmapExpiryAction :: Functor g => (a -> b) -> WithExpiryAction g a -> WithExpiryAction g b
fmapExpiryAction f (WithExpiryAction t ga) = WithExpiryAction t (fmap f ga)

packageDepCheck :: WithNewestDep r a -> (a -> g b) -> P.Sem r (WithExpiryAction g b)
packageDepCheck getDeps computeFromDeps = do
  (newestM, deps) <- P.runState Nothing getDeps 
  return $ WithExpiryAction newestM (computeFromDeps deps)

-- |
retrieveWD
  :: (P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     ,  K.LogWithPrefixesLE r
     , S.Serialize a)
  => T.Text
  -> WithNewestDep r a
retrieveWD k =  K.wrapPrefix ("Cache.retrieveWD (key=" <> k <> ")") $ do
  C.WithCacheTime ct a <- P.raise $ C.retrieveAndDecode knitSerialize Nothing k
  updateNewestTime ct
  return a
{-# INLINEABLE retrieveWD #-} 

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k.
-- ignores expiry
retrieveOrMake
  :: forall a r
   . ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text
  -> P.Sem r a
  -> P.Sem r a
retrieveOrMake k toMake =
  K.wrapPrefix ("Cache.retrieveOrMake (key=" <> k <> ")")
  $ fmap C.unWithCacheTime
  $ C.retrieveOrMake knitSerialize Nothing k toMake
{-# INLINEABLE retrieveOrMake #-}

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k.
-- remake if cached item is older than newest dependency
-- To use this we need the make-if-needed action to be one that
-- checks dependency times.
retrieveOrMakeWD
  :: forall a r
   . ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text
  -> P.Sem r (WithExpiryAction (P.Sem r) a)
  -> WithNewestDep r a
retrieveOrMakeWD k toMakeWD =  K.wrapPrefix ("Cache.retrieveOrMakeWD (key=" <> k <> ")") $ do
  (C.WithCacheTime cacheTime a) <- P.raise $ do
    WithExpiryAction newestM toMake <- toMakeWD -- don't run action.  Just get dependency info.
    C.retrieveOrMake knitSerialize newestM k toMake
  updateNewestTime cacheTime
  return a
{-# INLINEABLE retrieveOrMakeWD #-}


-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k.
-- Also has functions for mapping the input and output, often useful for
-- transforming something without a 'Serialize' instance into something with one.
-- ignore expiry
retrieveOrMakeTransformed
  :: forall a b r
   . ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize b
     )
  => (a -> b)
  -> (b -> a)
  -> T.Text
  -> P.Sem r a
  -> P.Sem r a
retrieveOrMakeTransformed toSerializable fromSerializable k toMake =
  K.wrapPrefix "retrieveOrMakeTransformed" $ 
  fmap fromSerializable $ retrieveOrMake k (fmap toSerializable toMake)
{-# INLINEABLE retrieveOrMakeTransformed #-}

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k.
-- Also has functions for mapping the input and output, often useful for
-- transforming something without a 'Serialize' instance into something with one.
retrieveOrMakeTransformedWD
  :: forall a b r
   . ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize b
     )
  => (a -> b)
  -> (b -> a)
  -> T.Text
  -> P.Sem r (WithExpiryAction (P.Sem r) a)
  -> WithNewestDep r a
retrieveOrMakeTransformedWD toSerializable fromSerializable k toMakeWD =
  K.wrapPrefix "retrieveOrMakeTransformed" $ 
  fmap fromSerializable $ retrieveOrMakeWD k (fmap (fmapExpiryAction toSerializable) toMakeWD)
{-# INLINEABLE retrieveOrMakeTransformedWD #-}


--
-- | Store a Streamly stream of @a@ at key k. Throw @PandocIOError@ on 'IOError'.
storeStream
  :: ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text
  -> Streamly.SerialT (P.Sem r) a
  -> P.Sem r ()
storeStream k aS = K.wrapPrefix ("Cache.storeStream key=" <> k <> ")") $ do
  K.logLE K.Diagnostic $ "Called with k=" <> k
  C.encodeAndStore knitSerializeStream k aS
{-# INLINEABLE storeStream #-}


-- | Retrieve a Streamly stream of @a@ from the store at key k. Throw if not found or 'IOError'
-- ignore dependency info
retrieveStream
  :: (P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize a)
  => T.Text
  -> Streamly.SerialT (P.Sem r) a
retrieveStream k =  Streamly.concatM
                    $ K.wrapPrefix ("Cache.retrieveStream (key=" <> k <> ")")
                    $ fmap C.unWithCacheTime
                    $ C.retrieveAndDecode knitSerializeStream Nothing k
{-# INLINEABLE retrieveStream #-}

-- | Retrieve a Streamly stream of @a@ from the store at key k. Throw if not found or 'IOError'
-- with dependency info
retrieveStreamWD
  :: (P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize a)
  => T.Text
  -> Streamly.SerialT (WithNewestDep r) a
retrieveStreamWD k =  Streamly.concatM
                    $ K.wrapPrefix ("Cache.retrieveStreamWD (key=" <> k <> ")")
                    $ do
  C.WithCacheTime ct a <- P.raise $ C.retrieveAndDecode knitSerializeStream Nothing k
  updateNewestTime ct
  return $ Streamly.hoist P.raise a
--  C.retrieveAndDecode knitSerializeStream k
{-# INLINEABLE retrieveStreamWD #-}


-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@. 
retrieveOrMakeStream
  :: forall a r
   . ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize a
     )
  => T.Text
  -> Streamly.SerialT (P.Sem r) a
  -> Streamly.SerialT (P.Sem r) a
retrieveOrMakeStream k toMake = Streamly.hoist (K.wrapPrefix $ "Cache.retrieveOrMakeStream (key=" <> k <> ")")
                                $ Streamly.concatM
                                $ fmap C.unWithCacheTime
                                $ C.retrieveOrMake knitSerializeStream Nothing k (return toMake)
{-# INLINEABLE retrieveOrMakeStream #-}

-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@. 
retrieveOrMakeStreamWD
  :: forall a r
   . ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize a
     )
  => T.Text
  -> P.Sem r (WithExpiryAction (Streamly.SerialT (P.Sem r)) a)
  -> Streamly.SerialT (WithNewestDep r) a
retrieveOrMakeStreamWD k toMakeWD = Streamly.concatM
                                    $ (K.wrapPrefix $ "Cache.retrieveOrMakeStream (key=" <> k <> ")")
                                    $ do
  (C.WithCacheTime cacheTime a) <- P.raise $ do
    WithExpiryAction newestM toMake <- toMakeWD -- don't run action.  Just get dependency info.
    C.retrieveOrMake knitSerializeStream newestM k (return toMake)
  updateNewestTime cacheTime
  return $ Streamly.hoist P.raise a
{-# INLINEABLE retrieveOrMakeStreamWD #-}



-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@.
-- Includes functions to map the stream items before encoding and after decoding.
retrieveOrMakeTransformedStream
  :: forall a b r
   . ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize b
     )
  => (a -> b)
  -> (b -> a)
  -> T.Text
  -> Streamly.SerialT (P.Sem r) a
  -> Streamly.SerialT (P.Sem r) a
retrieveOrMakeTransformedStream toSerializable fromSerializable k toMake =
  Streamly.hoist (K.wrapPrefix $ "retrieveOrMakeTransformedStream (key=" <> k <> ")")
  $ Streamly.map fromSerializable
  $ retrieveOrMakeStream k (Streamly.map toSerializable toMake)
{-# INLINEABLE retrieveOrMakeTransformedStream #-}


-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@.
-- Includes functions to map the stream items before encoding and after decoding.
retrieveOrMakeTransformedStreamWD
  :: forall a b r
   . ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize b
     )
  => (a -> b)
  -> (b -> a)
  -> T.Text
  -> P.Sem r (WithExpiryAction (Streamly.SerialT (P.Sem r)) a)
  -> Streamly.SerialT (WithNewestDep r) a
retrieveOrMakeTransformedStreamWD toSerializable fromSerializable k toMakeWD =
  Streamly.hoist (K.wrapPrefix $ "retrieveOrMakeTransformedStream (key=" <> k <> ")")
  $ Streamly.map fromSerializable
  $ retrieveOrMakeStreamWD k (fmap (mapExpiryAction (Streamly.map toSerializable)) toMakeWD)
{-# INLINEABLE retrieveOrMakeTransformedStreamWD #-}



fixMonadCatch :: (P.MemberWithError (P.Error Exceptions.SomeException) r)
              => Streamly.SerialT (Exceptions.CatchT (K.Sem r)) a -> Streamly.SerialT (K.Sem r) a
fixMonadCatch = Streamly.hoist f where
  f :: forall r a. (P.MemberWithError (P.Error Exceptions.SomeException) r) =>  Exceptions.CatchT (K.Sem r) a -> K.Sem r a
  f = join . fmap P.fromEither . Exceptions.runCatchT
{-# INLINEABLE fixMonadCatch #-}

{-
-- | Retrieve an a from the store at key k. Throw if not found or IOError
retrieveStream
  :: (P.Members '[KnitCache, P.Error PandocError, P.Embed IO] r, S.Serialize a)
  => T.Text
  -> Streamly.SerialT (P.Sem r) a
retrieveStream k = P.mapError ioErrorToPandocError $ C.retrieve cerealStream k
{-# INLINEABLE retrieve #-}

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k. 
retrieveOrMake
  :: forall a r
   . ( P.Members '[KnitCache, P.Error PandocError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text
  -> P.Sem r a
  -> P.Sem r a
retrieveOrMake k toMake = K.wrapPrefix "knitRetrieveOrMake" $ do
  ma <- C.retrieveMaybe cerealStreamly k
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
{-# INLINEABLE retrieveOrMake #-}

retrieveOrMakeTransformed
  :: forall a b r
   . ( P.Members '[KnitCache, P.Error PandocError, P.Embed IO] r
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
{-# INLINEABLE retrieveOrMakeTransformed #-}
-}
{-
-- | Clear the @b@ stored at key k.
clear :: K.KnitEffects r => T.Text -> P.Sem r ()
clear k = P.mapError ioErrorToPandocError $ C.clear k
{-# INLINEABLE clear #-}

ioErrorToPandocError :: IE.IOError -> PandocError
ioErrorToPandocError e = PandocIOError (K.textToPandocText $ ("IOError: " <> (T.pack $ show e)) e
{-# INLINEABLE ioErrorToPandocError #-}
-}
