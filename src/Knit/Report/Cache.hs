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

This module adds types, combinators and knit functions for using knit-haskell with the 'Knit.Effect.AtomicCache' effect
to implement a disk-persisted, in-memory key-value store with some support dependency tracking. The
knit-haskell stack uses "Text" keys, performs binary serialization using "Cereal" and uses 'Streamly.Memory.Array' from
"Streamly" as values for the in-memory Map.

For details about the cache itself, please see the documentation for 'Knit.Effect.AtomicCache'.

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.
-}
module Knit.Report.Cache
  (
    -- * Types
    CacheData
  , KnitCache
    -- * Dependency Tracking
  , WithCacheTime
  , ActionWithCacheTime
  , ignoreCacheTime
  , ignoreCacheTimeM
  , withCacheTime
  , onlyCacheTime
    -- * Cache Combinators
  , store  
  , clear
  , clearIfPresent
  , retrieve
  , retrieveOrMake
  , retrieveOrMakeTransformed
    -- * Streamly-Based
    -- ** Dependency Tracking
  , StreamWithCacheTime
  , ignoreCacheTimeStream
    -- ** Interoperation with non-stream actions
  , streamToAction
  , streamAsAction
    -- ** Cache Combinators
  , storeStream
  , retrieveStream
  , retrieveOrMakeStream
  , retrieveOrMakeTransformedStream 
    -- * Re-Exports
  , UTCTime
  )
where

import qualified Knit.Effect.AtomicCache       as C
import           Knit.Effect.AtomicCache        (clear
                                                , clearIfPresent
                                                , WithCacheTime                                                
                                                , withCacheTime
                                                , ignoreCacheTime
                                                , ignoreCacheTimeM
                                                , ActionWithCacheTime
                                                , onlyCacheTime)
import qualified Knit.Effect.Logger            as K

import           Control.Monad (join)
import qualified Control.Monad.Catch.Pure      as Exceptions

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor.Identity          (Identity(..))
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import           Data.Time.Clock                (UTCTime)
import qualified Data.Serialize                as S
import qualified Data.Word                     as Word

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P

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


-- | type used by the AtomicCache for in-memory storage.
type CacheData = Streamly.Array.Array Word.Word8

-- | AtomicCache with 'T.Text' keys and using 'Streamly.Array.Array Word.Word8' to store serialized data in memory.
type KnitCache = C.Cache T.Text CacheData


-- | Serialize a Serializable structure to the CacheData type.
knitSerialize :: ( S.Serialize a
                 , P.Member (P.Embed IO) r
                 , P.MemberWithError (P.Error C.CacheError) r
                 )
              => C.Serialize r a CacheData
knitSerialize = cerealArray
{-# INLINEABLE knitSerialize #-}

-- | Serialize a Streamly stream of Serializable structures to the CacheData type.
knitSerializeStream :: (S.Serialize a
                       , P.Member (P.Embed IO) r                
                       , P.MemberWithError (P.Error Exceptions.SomeException) r
                       , P.MemberWithError (P.Error C.CacheError) r
                       )
                       => C.Serialize r (Streamly.SerialT (P.Sem r) a) CacheData
knitSerializeStream = cerealStreamViaListArray
{-# INLINEABLE knitSerializeStream #-}

-- | Store an @a@ (serialized) at key k. Throw PandocIOError on IOError.
store
  :: ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text -- ^ Key
  -> a      -- ^ (Serializable) Data to store
  -> P.Sem r ()
store k a = K.wrapPrefix ("Knit.store (key=" <> k <> ")") $ do
  K.logLE (K.Debug 3) $ "Called with k=" <> k 
  C.encodeAndStore knitSerialize k a
{-# INLINEABLE store #-}

-- | Retrieve an @a@ from the store at key. Throw if not found or I/O Error.
retrieve
  :: (P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     ,  K.LogWithPrefixesLE r
     , S.Serialize a)
  => T.Text                              -- ^ Key
  -> P.Sem r (C.ActionWithCacheTime r a) -- ^ Time-stamped return from cache.
retrieve k =  K.wrapPrefix ("Cache.retrieve (key=" <> k <> ")")
              $ C.retrieveAndDecode knitSerialize k Nothing
{-# INLINEABLE retrieve #-}

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k.
retrieveOrMake
  :: ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text                              -- ^ Key
  -> C.ActionWithCacheTime r b           -- ^ Cached dependencies with time-stamp
  -> (b -> P.Sem r a)                    -- ^ Computation to produce @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (C.ActionWithCacheTime r a) -- ^ Time-stamped return from cache.
retrieveOrMake k cachedDeps toMake =
  K.wrapPrefix ("Cache.retrieveOrMake (key=" <> k <> ")")
  $ C.retrieveOrMake knitSerialize k cachedDeps toMake
{-# INLINEABLE retrieveOrMake #-}

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k.
-- Also has functions for mapping the input and output: useful for
-- caching something without a 'Serialize' instance but which is isomorphic to
-- something with one.
retrieveOrMakeTransformed
  :: ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , S.Serialize b
     )
  => (a -> b)                            -- ^ Transform @a@ to Serializable @b@
  -> (b -> a)                            -- ^ Transform Serializable @b@ to @a@
  -> T.Text                              -- ^ Key
  -> C.ActionWithCacheTime r c           -- ^ Cached dependencies with time-stamp
  -> (c -> P.Sem r a)                    -- ^ Computation to produce @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (C.ActionWithCacheTime r a) -- ^ Time-stamped @a@ from cache.
retrieveOrMakeTransformed toSerializable fromSerializable k newestM toMake =
  K.wrapPrefix "retrieveOrMakeTransformed"
  $ fmap (fmap fromSerializable)
  $ retrieveOrMake k newestM (fmap toSerializable . toMake)
{-# INLINEABLE retrieveOrMakeTransformed #-}

--
-- | Store a Streamly stream of @a@ at key k. Throw @PandocIOError@ on 'IOError'.
storeStream
  :: ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , K.LogWithPrefixesLE r
     , S.Serialize a
     )
  => T.Text                       -- ^ Key
  -> Streamly.SerialT (P.Sem r) a -- ^ Streamly stream to store
  -> P.Sem r ()
storeStream k aS = K.wrapPrefix ("Cache.storeStream key=" <> k <> ")") $ do
  K.logLE(K.Debug 3) $ "Called with k=" <> k
  C.encodeAndStore knitSerializeStream k aS
{-# INLINEABLE storeStream #-}

-- | Specify a Streamly Stream as the action in a 'C.WithCacheTime'
type StreamWithCacheTime r a = C.WithCacheTime (Streamly.SerialT (P.Sem r)) a

-- | Use a function from a @Stream (Sem r) a@  to @Sem r a@ to map from a stream action to a plain action over Sem. 
streamToAction :: (Streamly.SerialT (P.Sem r) a -> P.Sem r b) -> StreamWithCacheTime r a -> C.ActionWithCacheTime r b
streamToAction = C.wctMapAction
{-# INLINEABLE streamToAction #-}

-- | Wrap a stream action in @Sem r@ to make a stream action into a plain one holding the (still effectful) stream.
streamAsAction :: StreamWithCacheTime r a -> C.ActionWithCacheTime r (Streamly.SerialT (P.Sem r) a)
streamAsAction = streamToAction return
{-# INLINEABLE streamAsAction #-}

-- | Wrapper for AtomicCache.ignoreCacheTime, plus the concatM bit for streamly
ignoreCacheTimeStream :: P.Sem r (StreamWithCacheTime r a) -> Streamly.SerialT (P.Sem r) a
ignoreCacheTimeStream = Streamly.concatM . fmap C.ignoreCacheTime
{-# INLINEABLE ignoreCacheTimeStream #-}

-- | Retrieve a Streamly stream of @a@ from the store at key k. Throw if not found or 'IOError'
-- ignore dependency info
retrieveStream
  :: (P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize a)
  => T.Text                            -- ^ Key
  -> Maybe Time.UTCTime                -- ^ Cached item invalidation time.  Supply @Nothing@ to retrieve regardless of time-stamp.
  -> P.Sem r (StreamWithCacheTime r a) -- ^ Time-stamped stream from cache.
retrieveStream k newestM =  K.wrapPrefix ("Cache.retrieveStream (key=" <> k <> ")")
                            $ fmap (C.wctMapAction Streamly.concatM)
                            $ C.retrieveAndDecode knitSerializeStream k newestM
{-# INLINEABLE retrieveStream #-}

-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@. 
retrieveOrMakeStream
  :: ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize a
     )
  => T.Text                              -- ^ Key 
  -> C.ActionWithCacheTime r b           -- ^ Cached dependencies with time-stamp
  -> (b -> Streamly.SerialT (P.Sem r) a) -- ^ Computation to produce Stream of @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (StreamWithCacheTime r a)   -- ^ Time-stamped stream.
retrieveOrMakeStream k cachedDeps toMake = K.wrapPrefix ("Cache.retrieveOrMakeStream (key=" <> k <> ")")
                                           $ fmap (C.wctMapAction Streamly.concatM)
                                           $ C.retrieveOrMake knitSerializeStream k cachedDeps (return . toMake)
{-# INLINEABLE retrieveOrMakeStream #-}

-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@.
-- Also has functions for mapping the input and output: useful for
-- caching something without a 'Serialize' instance but which is isomorphic to
-- something with one.
retrieveOrMakeTransformedStream
  :: ( P.Members '[KnitCache, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , S.Serialize b
     )
  => (a -> b)                            -- ^ Transform @a@ to Serializable @b@
  -> (b -> a)                            -- ^ Transform Serializable @b@ to @a@
  -> T.Text                              -- ^ Key 
  -> C.ActionWithCacheTime r c           -- ^ Cached dependencies with time-stamp
  -> (c -> Streamly.SerialT (P.Sem r) a) -- ^ Computation to produce Stream of @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (StreamWithCacheTime r a)   -- ^ Time-stamped stream.
retrieveOrMakeTransformedStream toSerializable fromSerializable k cachedDeps toMake =
  K.wrapPrefix ("retrieveOrMakeTransformedStream (key=" <> k <> ")")
  $ fmap (C.wctMapAction $ Streamly.map fromSerializable)
  $ retrieveOrMakeStream k cachedDeps (Streamly.map toSerializable . toMake)
{-# INLINEABLE retrieveOrMakeTransformedStream #-}

-- | Use Pure CatchT to handle MonadCatch constraint
fixMonadCatch :: (P.MemberWithError (P.Error Exceptions.SomeException) r)
              => Streamly.SerialT (Exceptions.CatchT (P.Sem r)) a -> Streamly.SerialT (P.Sem r) a
fixMonadCatch = Streamly.hoist f where
  f :: forall r a. (P.MemberWithError (P.Error Exceptions.SomeException) r) =>  Exceptions.CatchT (P.Sem r) a -> P.Sem r a
  f = join . fmap P.fromEither . Exceptions.runCatchT
{-# INLINEABLE fixMonadCatch #-}

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
