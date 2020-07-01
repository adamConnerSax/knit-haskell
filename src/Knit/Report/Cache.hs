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
    -- * Dependency Tracking
   WithCacheTime
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
import qualified Knit.Effect.Serialize         as KS                 
import qualified Knit.Effect.Logger            as K

import qualified Control.Monad.Catch.Pure      as Exceptions

import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import           Data.Time.Clock                (UTCTime)

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P

import qualified Streamly                      as Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Internal.Prelude     as Streamly


-- | type used by the AtomicCache for in-memory storage.
-- type CacheData = Streamly.Array.Array Word.Word8

-- | AtomicCache with 'T.Text' keys and using 'Streamly.Array.Array Word.Word8' to store serialized data in memory.
--type KnitCache ct = C.Cache T.Text ct

serializationToCacheError :: KS.SerializationError -> C.CacheError
serializationToCacheError (KS.SerializationError msg) = C.DeSerializationError msg

mapSerializationErrorsOne ::
  P.MemberWithError (P.Error C.CacheError) r
  => KS.Serialize KS.SerializationError (P.Error KS.SerializationError ': r) a ct
  -> KS.Serialize C.CacheError r a ct
mapSerializationErrorsOne (KS.Serialize encode decode encBytes) =
  let f = P.mapError serializationToCacheError
  in KS.Serialize
  (f . encode)
  (f . decode)
  encBytes

-- | Serialize a Serializable structure to the CacheData type.
knitSerialize
  :: ( sc a
     , P.Member (P.Embed IO) r
     , P.MemberWithError (P.Error C.CacheError) r
     )
  => KS.SerializeDict sc ct
  -> KS.Serialize C.CacheError r a ct
knitSerialize = mapSerializationErrorsOne . KS.serializeOne --KS.cerealStreamlyDict
{-# INLINEABLE knitSerialize #-}

mapSerializationErrorsStreamly ::
  P.MemberWithError (P.Error C.CacheError) r
  => KS.Serialize KS.SerializationError (P.Error KS.SerializationError ': r) (Streamly.SerialT (P.Sem (P.Error KS.SerializationError ': r)) a) ct
  -> KS.Serialize C.CacheError r (Streamly.SerialT (P.Sem r) a) ct
mapSerializationErrorsStreamly (KS.Serialize encode decode encBytes) =
  let f =  P.mapError serializationToCacheError
  in KS.Serialize
     (f . fmap (\(ct, a) -> (ct, Streamly.hoist f a)) . encode . Streamly.hoist P.raise)
     (f . fmap (Streamly.hoist f) . decode)
     encBytes

-- | Serialize a Streamly stream of Serializable structures to the CacheData type.
knitSerializeStream :: (sc [a]
                       , P.Member (P.Embed IO) r                
                       , P.MemberWithError (P.Error C.CacheError) r
                       )
                       => KS.SerializeDict sc ct
                       -> KS.Serialize C.CacheError r (Streamly.SerialT (P.Sem r) a) ct
knitSerializeStream = mapSerializationErrorsStreamly . KS.serializeStreamlyViaList --KS.cerealStreamlyDict
{-# INLINEABLE knitSerializeStream #-}

-- | Store an @a@ (serialized) at key k. Throw PandocIOError on IOError.
store
  :: forall sc ct k r a.
     ( P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , Show k
     , sc a
     )
  => k -- ^ Key
  -> a -- ^ (Serializable) Data to store
  -> P.Sem r ()
store k a = K.wrapPrefix ("Knit.store (key=" <> (T.pack $ show k) <> ")") $ do
  cacheSD <- KS.getSerializeDict
  K.logLE (K.Debug 3) $ "Called with k=" <> (T.pack $ show k)
  C.encodeAndStore (knitSerialize cacheSD) k a
{-# INLINEABLE store #-}

-- | Retrieve an @a@ from the store at key. Throw if not found or I/O Error.
retrieve
  :: forall sc ct k r a.
  (P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  ,  K.LogWithPrefixesLE r
  , Show k
  , sc a)
  => k                                   -- ^ Key
  -> P.Sem r (C.ActionWithCacheTime r a) -- ^ Time-stamped return from cache.
retrieve k =  K.wrapPrefix ("Cache.retrieve (key=" <> (T.pack $ show k) <> ")") $ do
  cacheSD <- KS.getSerializeDict
  C.retrieveAndDecode (knitSerialize cacheSD) k Nothing
{-# INLINEABLE retrieve #-}

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k.
retrieveOrMake
  :: forall sc ct k r a b.
  ( P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  , K.LogWithPrefixesLE r
  , Show k
  , sc a     
  )
  => k                                   -- ^ Key
  -> C.ActionWithCacheTime r b           -- ^ Cached dependencies with time-stamp
  -> (b -> P.Sem r a)                    -- ^ Computation to produce @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (C.ActionWithCacheTime r a) -- ^ Time-stamped return from cache.
retrieveOrMake k cachedDeps toMake =
  K.wrapPrefix ("Cache.retrieveOrMake (key=" <> (T.pack $ show k) <> ")") $ do
   cacheSD <- KS.getSerializeDict
   C.retrieveOrMake (knitSerialize cacheSD) k cachedDeps toMake
{-# INLINEABLE retrieveOrMake #-}

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k.
-- Also has functions for mapping the input and output: useful for
-- caching something without a 'Serialize' instance but which is isomorphic to
-- something with one.
retrieveOrMakeTransformed
  :: forall sc ct k r a b c.
  ( P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  , K.LogWithPrefixesLE r
  , Show k
  , sc b
  )
  => (a -> b)                            -- ^ Transform @a@ to Serializable @b@
  -> (b -> a)                            -- ^ Transform Serializable @b@ to @a@
  -> k                                   -- ^ Key
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
  :: forall sc ct k r a.
  ( P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  , P.MemberWithError (P.Error Exceptions.SomeException) r
  , K.LogWithPrefixesLE r
  , Show k
  , sc [a]
  )
  => k                            -- ^ Key
  -> Streamly.SerialT (P.Sem r) a -- ^ Streamly stream to store
  -> P.Sem r ()
storeStream k aS = K.wrapPrefix ("Cache.storeStream key=" <> (T.pack $ show k) <> ")") $ do
  K.logLE(K.Debug 3) $ "Called with k=" <> (T.pack $ show k)
  cacheSD <- KS.getSerializeDict
  C.encodeAndStore (knitSerializeStream cacheSD) k aS
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
  :: forall sc k ct r a.
  (P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  , K.LogWithPrefixesLE r
  , P.MemberWithError (P.Error Exceptions.SomeException) r
  , Show k
  , sc [a])
  => k                                 -- ^ Key
  -> Maybe Time.UTCTime                -- ^ Cached item invalidation time.  Supply @Nothing@ to retrieve regardless of time-stamp.
  -> P.Sem r (StreamWithCacheTime r a) -- ^ Time-stamped stream from cache.
retrieveStream k newestM =  K.wrapPrefix ("Cache.retrieveStream (key=" <> (T.pack $ show k) <> ")") $ do
  cacheSD <- KS.getSerializeDict
  fmap (C.wctMapAction Streamly.concatM)
    $ C.retrieveAndDecode (knitSerializeStream cacheSD) k newestM
{-# INLINEABLE retrieveStream #-}

-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@. 
retrieveOrMakeStream
  :: forall sc k ct r a b.
     ( P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , Show k
     , sc [a]
     )
  => k                                   -- ^ Key 
  -> C.ActionWithCacheTime r b           -- ^ Cached dependencies with time-stamp
  -> (b -> Streamly.SerialT (P.Sem r) a) -- ^ Computation to produce Stream of @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (StreamWithCacheTime r a)   -- ^ Time-stamped stream.
retrieveOrMakeStream k cachedDeps toMake = K.wrapPrefix ("Cache.retrieveOrMakeStream (key=" <> (T.pack $ show k) <> ")") $ do
  cacheSD <- KS.getSerializeDict
  fmap (C.wctMapAction Streamly.concatM)
    $ C.retrieveOrMake (knitSerializeStream cacheSD) k cachedDeps (return . toMake)
{-# INLINEABLE retrieveOrMakeStream #-}

-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@.
-- Also has functions for mapping the input and output: useful for
-- caching something without a 'Serialize' instance but which is isomorphic to
-- something with one.
retrieveOrMakeTransformedStream
  :: forall sc ct k r a b c.
  ( P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  , K.LogWithPrefixesLE r
  , P.MemberWithError (P.Error Exceptions.SomeException) r
  , Show k
  , sc [b]
  )
  => (a -> b)                            -- ^ Transform @a@ to Serializable @b@
  -> (b -> a)                            -- ^ Transform Serializable @b@ to @a@
  -> k                                   -- ^ Key 
  -> C.ActionWithCacheTime r c           -- ^ Cached dependencies with time-stamp
  -> (c -> Streamly.SerialT (P.Sem r) a) -- ^ Computation to produce Stream of @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (StreamWithCacheTime r a)   -- ^ Time-stamped stream.
retrieveOrMakeTransformedStream toSerializable fromSerializable k cachedDeps toMake =
  K.wrapPrefix ("retrieveOrMakeTransformedStream (key=" <> (T.pack $ show k) <> ")")
  $ fmap (C.wctMapAction $ Streamly.map fromSerializable)
  $ retrieveOrMakeStream k cachedDeps (Streamly.map toSerializable . toMake)
{-# INLINEABLE retrieveOrMakeTransformedStream #-}
