{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
  , wctBind
    -- * Cache Combinators
  , store
  , clear
  , clearIfPresent
  , retrieve
  , retrieveOrMake
  , retrieveOrMakeTransformed
  -- * Utilities
  , fileDependency
  , updateIf
  , loadOrMakeFile
  , oldestUnit
    -- * Re-Exports
  , UTCTime
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
                                                , wctBind
                                                , ActionWithCacheTime
                                                , onlyCacheTime)
import qualified Knit.Effect.Serialize         as KS
import qualified Knit.Effect.Logger            as K
import qualified Knit.Utilities.Streamly       as KStreamly

import qualified Control.Exception as EX

import qualified Data.Time.Clock               as Time
import           Data.Time.Clock                (UTCTime)

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P

#if MIN_VERSION_streamly(0,9,0)
import qualified Streamly.Data.Stream as Streamly
#elif MIN_VERSION_streamly(0,8,0)
import qualified Streamly.Prelude as Streamly
#else
import qualified Streamly
#endif

import qualified System.Directory as System
import qualified System.IO.Error as SE


-- | type used by the AtomicCache for in-memory storage.
-- type CacheData = Streamly.Array.Array Word.Word8

-- | AtomicCache with 'T.Text' keys and using 'Streamly.Array.Array Word.Word8' to store serialized data in memory.
--type KnitCache ct = C.Cache T.Text ct

serializationToCacheError :: forall k.Show k => k -> KS.SerializationError -> C.CacheError
serializationToCacheError key (KS.SerializationError msg) = C.DeSerializationError $ "(key=" <> show key <> "): " <> msg

mapSerializationErrorsOne ::
  forall r k a ct.(P.Member (P.Error C.CacheError) r, Show k)
  => k
  -> KS.Serialize KS.SerializationError (P.Error KS.SerializationError ': r) a ct
  -> KS.Serialize C.CacheError r a ct
mapSerializationErrorsOne key (KS.Serialize encode decode encBytes) =
  let f :: P.Sem (P.Error KS.SerializationError ': r) x -> P.Sem r x
      f = P.mapError (serializationToCacheError key)
  in KS.Serialize
  (f . encode)
  (f . decode)
  encBytes

-- | Serialize a Serializable structure to the CacheData type.
knitSerialize
  :: ( sc a
     , P.Member (P.Embed IO) r
     , K.LogWithPrefixesLE r
     , P.Member (P.Error C.CacheError) r
     , Show k
     )
  => k
  -> KS.SerializeDict sc ct
  -> KS.Serialize C.CacheError r a ct
knitSerialize key = mapSerializationErrorsOne key . KS.serializeOne --KS.cerealStreamlyDict
{-# INLINEABLE knitSerialize #-}

#if MIN_VERSION_streamly(0,9,0)
mapSerializationErrorsStreamly ::
  forall r k a ct.(P.Member (P.Error C.CacheError) r, Show k)
  => k
  -> KS.Serialize KS.SerializationError (P.Error KS.SerializationError ': r) (Streamly.Stream KStreamly.StreamlyM a) ct
  -> KS.Serialize C.CacheError r (Streamly.Stream KStreamly.StreamlyM a) ct
#else
mapSerializationErrorsStreamly ::
  forall r k a ct.(P.Member (P.Error C.CacheError) r, Show k)
  => k
  -> KS.Serialize KS.SerializationError (P.Error KS.SerializationError ': r) (Streamly.SerialT KStreamly.StreamlyM a) ct
  -> KS.Serialize C.CacheError r (Streamly.SerialT KStreamly.StreamlyM a) ct
#endif
mapSerializationErrorsStreamly key (KS.Serialize encode decode encBytes) =
  let f :: P.Sem (P.Error KS.SerializationError ': r) x -> P.Sem r x
      f =  P.mapError (serializationToCacheError key)
  in KS.Serialize
     (f . encode)
     (f . decode)
     encBytes
{-# INLINEABLE mapSerializationErrorsStreamly #-}

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
  K.khDebugLog $ "Called with k=" <> show k
  C.encodeAndStore (knitSerialize k cacheSD) k a
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
  C.retrieveAndDecode (knitSerialize k cacheSD) k Nothing
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
   C.retrieveOrMake (knitSerialize k cacheSD) k cachedDeps toMake
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

-- | Create a cached (), (@ActionWithCacheTime r ()@) to use as a dependency from a
-- foldable of FilePaths, using the semigroup instance of @ActionWithCacheTime@

-- If any of the files do not exist, the cache time will be Nothing, which will cause anything
-- using this as a dependency to rebuild.
-- This is intended for use when some function should be re-run when when some file,
-- presumably a side-effect of that function, is older than some dependency.
filesDependency :: (Foldable f, P.Member (P.Embed IO) r)
               => f FilePath
               -> P.Sem r (ActionWithCacheTime r ())
filesDependency fps = do
  fileDeps <- traverse fileDependency $ Foldl.fold Foldl.list fps
  return $ fromMaybe (pure ()) $ fmap sconcat $ nonEmpty fileDeps
{-# INLINEABLE filesDependency #-}
-- TODO: Check what happens if a file is missing!

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
{-# INLINEABLE oldestUnit #-}
