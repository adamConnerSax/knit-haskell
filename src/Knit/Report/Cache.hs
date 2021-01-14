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
{-# OPTIONS_GHC -O2 -fdicts-strict -fspec-constr-recursive=16 -fmax-worker-args=16 #-} -- for streamly
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
  , liftActionWithCacheTime
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
  , mapCachedStream
  , runCachedStream
  , runCachedStreamM
    -- ** Interoperation with non-stream actions
  , streamToAction
  , streamAsAction
    -- ** Cache Combinators
  , storeStream
  , retrieveStream
  , retrieveOrMakeStream
  , retrieveOrMakeTransformedStream
    -- * Utilities
  , fileDependency
  , updateIf
  , loadOrMakeFile
  , oldestUnit
    -- * Re-Exports
  , UTCTime
  -- * temp
  , actionWCT2StreamWCT
  , knitSerialize
  , knitSerializeStream
  )
where

import qualified Control.Foldl as Foldl
import qualified Knit.Effect.AtomicCache       as C
import           Knit.Effect.AtomicCache        (clear
                                                , clearIfPresent
                                                , WithCacheTime
                                                , withCacheTime
                                                , ignoreCacheTime
                                                , ignoreCacheTimeM
                                                , liftActionWithCacheTime
                                                , ActionWithCacheTime
                                                , onlyCacheTime)
import qualified Knit.Effect.Serialize         as KS
import qualified Knit.Effect.Logger            as K
import qualified Knit.Utilities.Streamly       as KStreamly

import qualified Control.Monad.Catch.Pure      as Exceptions
import qualified Control.Exception as EX

import qualified Data.Time.Clock               as Time
import           Data.Time.Clock                (UTCTime)

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P

import qualified Streamly
import qualified Streamly.Prelude              as Streamly

import qualified System.Directory as System
import qualified System.IO.Error as SE

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
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error C.CacheError) r
     )
  => KS.SerializeDict sc ct
  -> KS.Serialize C.CacheError r a ct
knitSerialize = mapSerializationErrorsOne . KS.serializeOne --KS.cerealStreamlyDict
{-# INLINEABLE knitSerialize #-}

mapSerializationErrorsStreamly ::
  P.MemberWithError (P.Error C.CacheError) r
  => KS.Serialize KS.SerializationError (P.Error KS.SerializationError ': r) (Streamly.SerialT KStreamly.StreamlyM a) ct
  -> KS.Serialize C.CacheError r (Streamly.SerialT KStreamly.StreamlyM a) ct
mapSerializationErrorsStreamly (KS.Serialize encode decode encBytes) =
  let f =  P.mapError serializationToCacheError
  in KS.Serialize
     (f . encode)
     (f . decode)
     encBytes

-- | Serialize a Streamly stream of Serializable structures to the CacheData type.
knitSerializeStream :: (sc a
                       , P.Member (P.Embed IO) r
                       , P.MemberWithError (P.Error C.CacheError) r
                       , K.LogWithPrefixesLE r
                       )
                       => KS.SerializeDict sc ct
                       -> KS.Serialize C.CacheError r (Streamly.SerialT KStreamly.StreamlyM a) ct
knitSerializeStream = mapSerializationErrorsStreamly . KS.serializeStreamly --KS.cerealStreamlyDict
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
store k a = K.wrapPrefix ("Knit.store (key=" <> show k <> ")") $ do
  cacheSD <- KS.getSerializeDict
  K.logLE (K.Debug 3) $ "Called with k=" <> show k
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
retrieve k =  K.wrapPrefix ("Cache.retrieve (key=" <> show k <> ")") $ do
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
  K.wrapPrefix ("Cache.retrieveOrMake (key=" <> show k <> ")") $ do
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
  $ fromSerializable <<$>> retrieveOrMake k newestM (fmap toSerializable . toMake)
{-# INLINEABLE retrieveOrMakeTransformed #-}

--
-- | Store a Streamly stream of @a@ at key k. Throw @PandocIOError@ on 'IOError'.
storeStream
  :: forall sc ct k r a.
  ( P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  , P.MemberWithError (P.Error Exceptions.SomeException) r
  , K.LogWithPrefixesLE r
  , Show k
  , sc a
  )
  => k                            -- ^ Key
  -> Streamly.SerialT KStreamly.StreamlyM a -- ^ Streamly stream to store
  -> P.Sem r ()
storeStream k aS = K.wrapPrefix ("Cache.storeStream key=" <> show k <> ")") $ do
  K.logLE(K.Debug 3) $ "Called with k=" <> show k
  cacheSD <- KS.getSerializeDict
  C.encodeAndStore (knitSerializeStream cacheSD) k aS
{-# INLINEABLE storeStream #-}

-- | Specify a Streamly Stream as the action in a 'C.WithCacheTime'
type StreamWithCacheTime a = C.WithCacheTime (Streamly.SerialT KStreamly.StreamlyM) a

-- | Apply a stream transformer to a cached stream
mapCachedStream :: (Streamly.SerialT KStreamly.StreamlyM a -> Streamly.SerialT KStreamly.StreamlyM b)
                -> StreamWithCacheTime a
                -> StreamWithCacheTime b
mapCachedStream = C.wctMapAction --C.withCacheTime (C.cacheTime swct) (f $ C.ignoreCacheTime swct)
{-# INLINEABLE mapCachedStream #-}

-- | Use a function from a @Stream StreamlyM a@  to @StreamlyM b@ to map from a stream action to a plain action, then lift into Sem.
streamToAction :: (P.Member (P.Embed IO) r
                  , K.LogWithPrefixesLE r
                  )
               => (Streamly.SerialT KStreamly.StreamlyM a -> KStreamly.StreamlyM b) -> StreamWithCacheTime a -> C.ActionWithCacheTime r b
streamToAction f = C.wctMapAction (KStreamly.streamlyToKnit . f)
{-# INLINEABLE streamToAction #-}

-- | Wrap a stream action in @Sem r@ to make a stream action into a plain one holding the (still effectful) stream.
streamAsAction :: (P.Member (P.Embed IO) r
                  , K.LogWithPrefixesLE r
                  ) => StreamWithCacheTime a -> C.ActionWithCacheTime r (Streamly.SerialT KStreamly.StreamlyM a)
streamAsAction = streamToAction return
{-# INLINEABLE streamAsAction #-}

runCachedStream :: (P.Member (P.Embed IO) r
                  , K.LogWithPrefixesLE r
                  )
                => (Streamly.SerialT KStreamly.StreamlyM a -> KStreamly.StreamlyM b)
                -> StreamWithCacheTime a
                -> C.ActionWithCacheTime r b
runCachedStream f swct =
  let t = C.cacheTime swct
      s = C.ignoreCacheTime swct
  in C.withCacheTime t (KStreamly.streamlyToKnit $ f s)
{-# INLINEABLE runCachedStream #-}

runCachedStreamM :: (P.Member (P.Embed IO) r
                  , K.LogWithPrefixesLE r
                  )
                 => (Streamly.SerialT KStreamly.StreamlyM a -> KStreamly.StreamlyM b)
                 -> P.Sem r (StreamWithCacheTime a)
                 -> P.Sem r b
runCachedStreamM f swctM = C.ignoreCacheTimeM $ fmap (runCachedStream f) swctM
{-# INLINEABLE runCachedStreamM #-}

{-
-- | Wrapper for AtomicCache.ignoreCacheTime, plus the concatM bit for streamly
ignoreCacheTimeStream :: P.Sem r (StreamWithCacheTime a) -> P.Sem r (Streamly.SerialT KStreamly.StreamlyM a)
ignoreCacheTimeStream = Streamly.concatM . fmap C.ignoreCacheTime
{-# INLINEABLE ignoreCacheTimeStream #-}
-}


actionWCT2StreamWCT :: (K.LogWithPrefixesLE r)
                    => P.Sem r (C.ActionWithCacheTime r (Streamly.SerialT KStreamly.StreamlyM a))
                    -> P.Sem r (StreamWithCacheTime a)
actionWCT2StreamWCT x = K.wrapPrefix "actionWCT2StreamWCT" $ x >>= \wct -> C.withCacheTime (C.cacheTime wct) <$> C.ignoreCacheTime wct
{-
  K.logLE (K.Debug 3) $ "Before wct is bound"
  wct <- x
  K.logLE (K.Debug 3) $ "After wct is bound"
  fmap (C.withCacheTime $ C.cacheTime wct) $ C.ignoreCacheTime wct
-}
{-# INLINEABLE actionWCT2StreamWCT #-}

-- | Retrieve a Streamly stream of @a@ from the store at key k. Throw if not found or 'IOError'
-- ignore dependency info
-- NB: This will deserialize when the return value is bound so this is somewhat less efficient as a
-- dependency.  As an alternative, use versions
retrieveStream
  :: forall sc k ct r a.
  (P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  , K.LogWithPrefixesLE r
  , P.MemberWithError (P.Error Exceptions.SomeException) r
  , Show k
  , sc a
  )
  => k                                 -- ^ Key
  -> Maybe Time.UTCTime                -- ^ Cached item invalidation time.  Supply @Nothing@ to retrieve regardless of time-stamp.
  -> P.Sem r (StreamWithCacheTime a) -- ^ Time-stamped stream from cache.
retrieveStream k newestM =  K.wrapPrefix ("Cache.retrieveStream (key=" <> show k <> ")") $ do
  cacheSD <- KS.getSerializeDict
  actionWCT2StreamWCT
    $ C.retrieveAndDecode (knitSerializeStream cacheSD) k newestM
{-# INLINEABLE retrieveStream #-}

{-
retrieveStream'
  :: forall sc k ct r a.
  (P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
  , K.LogWithPrefixesLE r
  , P.MemberWithError (P.Error Exceptions.SomeException) r
  , Show k
  , sc [a])
  => k                                 -- ^ Key
  -> Maybe Time.UTCTime                -- ^ Cached item invalidation time.  Supply @Nothing@ to retrieve regardless of time-stamp.
  -> P.Sem r (ActionWithCacheTime r (Streamly.SerialT KStreamly.StreamlyM a)) -- ^ Time-stamped stream from cache.
retrieveStream' k newestM =  K.wrapPrefix ("Cache.retrieveStream (key=" <> (T.pack $ show k) <> ")") $ do
  cacheSD <- KS.getSerializeDict
  C.retrieveAndDecode (knitSerializeStream cacheSD) k newestM
{-# INLINEABLE retrieveStream' #-}
-}

-- | Retrieve a Streamly stream of @a@ from the store at key @k@.
-- If retrieve fails then perform the action and store the resulting stream at key @k@.
retrieveOrMakeStream
  :: forall sc ct k r a b.
     ( P.Members '[KS.SerializeEnv sc ct, C.Cache k ct, P.Error C.CacheError, P.Embed IO] r
     , K.LogWithPrefixesLE r
     , P.MemberWithError (P.Error Exceptions.SomeException) r
     , Show k
     , sc a
     )
  => k                                   -- ^ Key
  -> C.ActionWithCacheTime r b           -- ^ Cached dependencies with time-stamp
  -> (b -> Streamly.SerialT KStreamly.StreamlyM a) -- ^ Computation to produce Stream of @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (StreamWithCacheTime a)   -- ^ Time-stamped stream.
retrieveOrMakeStream k cachedDeps toMake = K.wrapPrefix ("Cache.retrieveOrMakeStream (key=" <> show k <> ")") $ do
  cacheSD <- KS.getSerializeDict
  actionWCT2StreamWCT
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
  , sc b
  )
  => (a -> b)                            -- ^ Transform @a@ to Serializable @b@
  -> (b -> a)                            -- ^ Transform Serializable @b@ to @a@
  -> k                                   -- ^ Key
  -> C.ActionWithCacheTime r c           -- ^ Cached dependencies with time-stamp
  -> (c -> Streamly.SerialT KStreamly.StreamlyM a) -- ^ Computation to produce Stream of @a@ if absent from cache or cached version is older than dependencies.
  -> P.Sem r (StreamWithCacheTime a)   -- ^ Time-stamped stream.
retrieveOrMakeTransformedStream toSerializable fromSerializable k cachedDeps toMake =
  K.wrapPrefix ("retrieveOrMakeTransformedStream (key=" <> show k <> ")")
  $ C.wctMapAction (Streamly.map fromSerializable) <$> retrieveOrMakeStream k cachedDeps (Streamly.map toSerializable . toMake)
{-# INLINEABLE retrieveOrMakeTransformedStream #-}


-- | Create a cached (), (@ActionWithCacheTime r ()@) to use as a dependency from a FilePath.
-- If the file does not exist, the cache time will be Nothing, which will cause anything
-- using this as a dependency to rebuild.
-- This is intended for use when some function should be re-run when when some file,
-- presumably a side-effect of that function, is older than some dependency.
fileDependency :: P.Member (P.Embed IO) r
               => FilePath
               -> P.Sem r (ActionWithCacheTime r ())
fileDependency fp = do
  let checkError e = if SE.isDoesNotExistError e then Just () else Nothing
  modTimeE <- P.embed $ EX.tryJust checkError $ System.getModificationTime fp
  let modTimeM = case modTimeE of
        Left _ -> Nothing
        Right modTime -> Just modTime
  return $ withCacheTime modTimeM (return ())
{-# INLINEABLE fileDependency #-}


-- | Given a time-tagged @a@ and time-tagged @b@ and an effectful function
-- producing @b@ from @a@, return the given @b@ or run the function with the
-- given @a@, depending on the time-stamps of the given @a@ and @b@.
-- That is, if the given @b@ is newer than the given @a@, return it,
-- otherwise run the function to produce a new b.
-- E.g., can be used along with @fileDependency@ above to run a function
-- only when the given file is older than some given input dependency.
updateIf :: P.Member (P.Embed IO) r
         => ActionWithCacheTime r b
         -> ActionWithCacheTime r a
         -> (a -> P.Sem r b)
         -> P.Sem r (ActionWithCacheTime r b)
updateIf cur deps update = if C.cacheTime cur >= C.cacheTime deps then return cur else updatedAWCT where
  updatedAWCT = do
    updatedB <- ignoreCacheTime deps >>= update
    nowCT <- P.embed Time.getCurrentTime
    return $ withCacheTime (Just nowCT) (return updatedB)
{-# INLINEABLE updateIf #-}

-- | Given a file name and way to turn that file into type @b@, check for existence and then
-- check timestamp against input data.  Remake if too old or make if file is missing.
loadOrMakeFile ::  P.Member (P.Embed IO) r
               => FilePath  -- ^ path to file
               -> (FilePath -> P.Sem r b) -- ^ action to make result from file if it exists
               -> ActionWithCacheTime r a -- dependencies
               -> (a -> P.Sem r b) -- ^ action to make result from existing data
               -> P.Sem r (ActionWithCacheTime r b)
loadOrMakeFile fp loader deps maker = do
  fileDep <- fileDependency fp
  let fromFile = C.wctBind loader (fmap (const fp) fileDep)
      makeNew = do
        updatedB <- ignoreCacheTime deps >>= maker
        nowCT <- P.embed Time.getCurrentTime
        return $ withCacheTime (Just nowCT) (return updatedB)
  if isJust (C.cacheTime fileDep)
    then updateIf fromFile deps maker
    else makeNew
{-# INLINEABLE loadOrMakeFile #-}

-- | Utility for taking a set of time-tagged items and producing a single time-tagged unit.
-- Useful if you wish to run a function (using, e.g., @updateIf@) when any of a set of things is
-- too old an requires updating.
oldestUnit :: (Foldable f, Functor f, Applicative m) => f (WithCacheTime m w) -> WithCacheTime m ()
oldestUnit cts = withCacheTime t (pure ()) where
--  t = minimum $ fmap C.cacheTime cts
  t = join $ Foldl.fold Foldl.minimum $ fmap C.cacheTime cts

{-
  if null cts
    then Nothing
    else foldl' (\min x -> if x < min then x else min) Nothing $ fmap C.cacheTime cts
-}
{-# INLINEABLE oldestUnit #-}
