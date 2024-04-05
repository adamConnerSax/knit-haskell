{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -O2 -fdicts-strict -fspec-constr-recursive=16 -fmax-worker-args=16 #-} -- for Streamly
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-|
Module      : Knit.Effect.AtomicCache
Description : Effect for managing a persistent cache of serializable things to avoid redoing computations
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module defines a key/value store (Polysemy) effect. Rather than return the usual @Maybe v@ from a lookup,
the cache returns a @Maybe (WithCacheTime v)@, where @WithCacheTime a@ wraps a value of type a along with a
time-stamp (of type @UTCTime@). This module provides a thread-safe in-memory implementation as well as
a disk-based persistent implementation and a combination of the two, where the disk-based layer sits behind the
in-memory layer.  In our use case, the stored values will be arrays of bytes, the result of serializing
whatever data we wish to cache.

@WithCacheTime@ is intended to simplify tracking dependencies among cached computations.  For example, imagine
you have two long running computations which you wish to cache so you need only run those computations once:

@
computeA :: m a
computeA = ...

computeB :: m b
computeB = ...

cachedA :: WithCacheTime m a
cachedA :: retrieveOrMake serialize "a.bin" (pure ()) (const computeA)

cachedB :: WithCacheTime m b
cachedB = retrieveOrMake serialize "b.bin" (pure ()) (const computeB)
@

and you have a computation which depends on @a@ and @b@ and should also be cached, but we
want to make sure it gets recomputed if either @a@ or @b@ do. We use the applicative instance of
@WithCacheTime@ to combine cached results into and inject them into later computations while
taking into account the newest time-stamp among the dependencies:

@
computeC :: a -> b -> m c
computeC a b = ...

cDeps :: WithCachedTime m (a, b)
cDeps = (,) <$> cachedA \<*\> cachedB

cachedC :: WithCacheTime m c
cachedC = retrieveOrMake serialize "c.bin" cDeps $ \\(a, b) -> computeC a b
@

As with @cachedA@ and @cachedB@, @cachedC@ will run the computation if the key, "c.bin" in this case,
is absent from the cache.
In addition, @cachedC@ will be recomputed even if it is in the cache, if the time-stamp of the cached value
is older than either the time stamp of @cachedA@ or @cachedB@.

@WithCacheTime m a@ holds the time-stamp and a monadic computation which will produce an @a@. This allows
deferral of the deserialization of cached data until we know that we need to use it.  In the example above,
suppose @a@ is retrieved from cache, and @b@ is computed fresh.  @cachedA@ holds a timestamp
(the modification time of the file in cache or the time a was cached in memory) and a monadic
computation which will deserialize the cached byte array retrieved for a.  @cachedB@ holds a time-stamp
(the time the computation of b completes) and the trivial monadic action @return b@.  Since @b@ was
just computed, the cached @c@ is outdated and will be recomputed.  At that point @a@ is deserialized, @b@
is unwrapped and thse are given to the function to compute @c@, which is then
stored in cache as well as returned in the @WithCacheTime m c@, holding a new time-stamp.

If multiple threads attempt to lookup or 'retrieveOrMake' at the same key
at close to the same time, the first request will proceed,
loading from cache if possible, and the other threads will block until
the in-memory cache is populated or the first thread fails to fill in data.

This is intended to save CPU in the relatively common case that, e.g., several threads
are launched to analyze the same data.  The functions which load that data
from on-disk-cache or produce it from other analyses need only be run once.  Using the cache
to facilitate this sharing still requires each thread to deserialize the data.  If that cost is
significant, you may want to compute the data before launching the threads.

NB: Should the action given to create the data, the @(b -> m a)@ argument of 'retrieveOrMake' somehow
fail, this may lead to a situation where it runs on the first thread, fails, then runs on all the other threads
simultaneously, presumably failing all those times as well.

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.
-}
module Knit.Effect.AtomicCache
  (
    -- * Effect
    Cache
    -- * Time Stamps
    -- ** Types
  , WithCacheTime
--  , pattern WithCacheTime
  , ActionWithCacheTime
--  , pattern ActionWithCacheTime
    -- ** Constructors
  , withCacheTime
  , onlyCacheTime
    -- ** Combinators
  , ignoreCacheTime
  , ignoreCacheTimeM
  , cacheTime
  , liftActionWithCacheTime
    -- ** Utilities
  , wctMapAction
  , wctBind
  , wctMerge
  , wctMergeM
  , wctSplit
    -- ** Cache Actions
  , encodeAndStore
  , retrieveAndDecode
  , lookupAndDecode
  , retrieveOrMake
  , clear
  , clearIfPresent
    -- * Effect Interpretations
    -- ** Persist To Disk
  , persistStreamlyByteArray
  , persistLazyByteString
  , persistStrictByteString
    -- ** Thread-safe Map
  , AtomicMemCache
  , runAtomicInMemoryCache
    -- ** Combined Map/Disk
  , runBackedAtomicInMemoryCache
  , runPersistenceBackedAtomicInMemoryCache
  , runPersistenceBackedAtomicInMemoryCache'
    -- * logging
  , cacheLog
    -- * Exceptions
  , CacheError(..)
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import qualified Knit.Effect.Logger            as K
import qualified Knit.Effect.Internal.Logger   as K
import qualified Knit.Effect.Serialize         as KS

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Semigroup                as Semigroup
import qualified Data.Text                     as T
import qualified Data.Map as M
import qualified Data.Time.Clock               as Time
import qualified Data.Word                     as Word

import qualified Control.Concurrent.STM        as C
import qualified Control.Exception             as Exception

#if MIN_VERSION_streamly(0,9,0)
import qualified Streamly.Data.Array    as Streamly.Array
import qualified Streamly.Internal.Data.Array as Streamly.Array
#elif MIN_VERSION_streamly(0,8,0)
import qualified Streamly.Internal.Data.Array.Foreign    as Streamly.Array
#else
import qualified Streamly.Internal.Memory.Array    as Streamly.Array
#endif
import qualified Streamly.Internal.FileSystem.File as Streamly.File

import qualified System.Directory              as System
import qualified System.IO.Error               as IO.Error


{- TODO:
1. Can this design be simplified, part 1. The Maybe in the TMVar seems like it should be uneccessary.
2. It'd be nice to make sure we can't leave the empty TMVar. Can this be done in a way so that it must be filled?
3. We should be able to factor out some things around handling the returned TMVar
-}
-- | Error Type for Cache errors.  Simplifies catching and reporting them.
data CacheError =
  ItemNotFoundError Text
  | ItemTooOldError Text
  | DeSerializationError Text
  | PersistError Text
  | OtherCacheError Text deriving (Show, Eq)

-- | Wrapper to hold (deserializable, if necessary) content and a timestamp.
-- The stamp must be at or after the time the data was constructed
data Q w t a where
  Q :: w -> t a -> Q w t a

instance Functor t => Functor (Q w t) where
  fmap f (Q w ta) = Q w (fmap f ta)
  {-# INLINE fmap #-}

instance (Monoid w, Applicative t) => Applicative (Q w t) where
  pure a = Q mempty (pure a)
  {-# INLINE pure #-}
  (Q w1 t1) <*> (Q w2 t2) = Q (w1 <> w2) (t1 <*> t2)
  {-# INLINE (<*>) #-}

instance Foldable t => Foldable (Q w t) where
  foldMap f (Q _ ta) = foldMap f ta
  foldr f b (Q _ ta) = foldr f b ta

instance Traversable t => Traversable (Q w t) where
  traverse g (Q w ta) = Q w <$> traverse g ta
  sequenceA (Q w fta) = Q w <$> sequenceA fta

-- | Map one type of action to another via a natural transformation.
-- Specifically useful for mapping from @Q w Identity a@
-- to @Q w m a@
natQ :: (forall a. f a -> g a) -> Q w f b -> Q w g b
natQ nat (Q tM fb) = Q tM (nat fb)
{-# INLINEABLE natQ #-}

-- | Map one type of action to another.  NB: 'Q w m' is a functor
-- (as long as @m@ is), so if @m@ is not changing, you should prefer 'fmap'
-- to this function.
morphQ :: (m a -> n b) -> Q w m a -> Q w n b
morphQ f (Q tM ma) = Q tM (f ma)
{-# INLINEABLE morphQ #-}

actionOnlyQ :: Q w m a -> m a
actionOnlyQ (Q _ ma) = ma
{-# INLINEABLE actionOnlyQ #-}

monoidOnlyQ :: Q w m a -> w
monoidOnlyQ (Q w _) = w
{-# INLINEABLE monoidOnlyQ #-}

joinActionOnlyQ :: Monad m => m (Q w m a) -> m a
joinActionOnlyQ = (actionOnlyQ =<<)
{-# INLINEABLE joinActionOnlyQ #-}

mergeQ :: (Monad m, Semigroup w) => Q w m (Q w m a) -> m (Q w m a)
mergeQ (Q w mQ) = do
  (Q w' ma) <- mQ
  return $ Q (w <> w') ma
{-# INLINEABLE mergeQ #-}

mergeQM :: (Monad m, Semigroup w) => m (Q w m (Q w m a)) -> m (Q w m a)
mergeQM  x = x >>= mergeQ
{-# INLINEABLE mergeQM #-}

promoteQ :: Monad m => (a -> m b) -> Q w m a -> Q w m b
promoteQ f (Q w ma) = Q w (ma >>= f)
{-# INLINEABLE promoteQ #-}

chooseQ :: Ord w => Q w t a -> Q w t a -> Q w t a
chooseQ (Q w1 t1) (Q w2 t2) = if w1 >= w2 then Q w1 t1 else Q w2 t2
{-# INLINEABLE chooseQ #-}

productQ :: (Applicative t, Monoid w) => (Q w t a -> Q w t b) -> (Q w t a -> Q w t c) -> Q w t a -> Q w t (b, c)
productQ makeB makeC cachedA = (,) <$> makeB cachedA <*> makeC cachedA
{-# INLINEABLE productQ #-}

zipWithQ :: (Monad m, Monoid w)
         => (b -> d -> m e)
         -> (Q w m a -> Q w m b)
         -> (Q w m c -> Q w m d)
         -> Q w m (a, c)
         -> Q w m e
zipWithQ f bFromA dFromC depsAC = do
  let bQ = bFromA $ fmap fst depsAC
      dQ = dFromC $ fmap snd depsAC
      bdQ = (,) <$> bQ <*> dQ
  promoteQ (uncurry f) bdQ
{-# INLINEABLE zipWithQ #-}


splitQ :: (Monad m) => Q w m (a, b) -> m (Q w m a, Q w m b)
splitQ (Q w mab) = do
  (a, b) <- mab
  pure (Q w $ pure a, Q w $ pure b)
{-# INLINEABLE splitQ #-}
{-
sumQ :: Monad m => (Q w m a -> Q w m b) -> (Q w m c -> Q w m b) -> Q w m (Either a c) -> Q w m b
sumQ fromA fromC (Q depT mEither) = Q depT actionB where
  actionB = do
    e <- mEither
    case e of
      Left a -> actionOnlyQ $ fromA (Q depT (return a))
      Right c -> actionOnlyQ $ fromC (Q depT (return c))
-}

-- TODO: double check that this is associative
instance Ord w => Semigroup (Q w t a) where
  (<>) = chooseQ
  {-# INLINEABLE (<>) #-}

type TimeM = Maybe Time.UTCTime
type CacheTime = Maybe (Semigroup.Max Time.UTCTime)

toCacheTime :: TimeM -> CacheTime
toCacheTime = fmap Semigroup.Max

toTimeM :: CacheTime -> TimeM
toTimeM = fmap Semigroup.getMax

-- recall that @Ord a => Ord (Maybe a)@ with @Nothing <  Just a@ for all a.
-- So this specification of Q can be be used in chooseQ (and is a semigroup)
type WithCacheTime m a = Q CacheTime m a

pattern WithCacheTime :: CacheTime -> m a -> WithCacheTime m a
pattern WithCacheTime mTime ma <- Q mTime ma where
  WithCacheTime mTime ma = Q mTime ma

-- | Specialize `WithCacheTime` for use with a Polysemy effects stack.
type ActionWithCacheTime r a = WithCacheTime (P.Sem r) a

{-
pattern ActionWithCacheTime :: CacheTime -> P.Sem r a -> ActionWithCacheTime r a
pattern ActionWithCacheTime mTime ma <- Q mTime ma where
  ActionWithCacheTime mTime ma = Q mTime ma
-}
-- | Construct a WithCacheTime with a time and no action.
onlyCacheTime :: Applicative m => TimeM -> WithCacheTime m ()
onlyCacheTime tM = WithCacheTime (toCacheTime tM) (pass)
{-# INLINEABLE onlyCacheTime #-}

-- | Construct a WithCacheTime from a @Maybe Time.UTCTime@ and an action.
withCacheTime :: TimeM -> m a -> WithCacheTime m a
withCacheTime tM = WithCacheTime (toCacheTime tM)
{-# INLINEABLE withCacheTime #-}

-- | Map one type of action to another via a natural transformation.
-- Specifically useful for mapping from @WithCacheTime Identity a@
-- to @WithCacheTime m a@
wctApplyNat :: (forall a. f a -> g a) -> WithCacheTime f b -> WithCacheTime g b
wctApplyNat = natQ
{-# INLINEABLE wctApplyNat #-}

-- | Map one type of action to another.  NB: 'WithCacheTime m' is a functor
-- (as long as @m@ is), so if @m@ is not changing, you should prefer 'fmap'
-- to this function.
wctMapAction :: (m a -> n b) -> WithCacheTime m a -> WithCacheTime n b
wctMapAction = morphQ
{-# INLINEABLE wctMapAction #-}

-- | Promote a monadic function to a function between Cached actions.
wctBind :: Monad m => (a -> m b) -> WithCacheTime m a -> WithCacheTime m b
wctBind = promoteQ
{-# INLINEABLE wctBind #-}

wctMerge :: Monad m => WithCacheTime m (WithCacheTime m a) -> m (WithCacheTime m a)
wctMerge = mergeQ
{-# INLINEABLE wctMerge #-}

wctMergeM :: Monad m => m (WithCacheTime m (WithCacheTime m a)) -> m (WithCacheTime m a)
wctMergeM = mergeQM
{-# INLINEABLE wctMergeM #-}

wctSplit :: Monad m => WithCacheTime m (a, b) -> m (WithCacheTime m a, WithCacheTime m b)
wctSplit = splitQ
{-# INLINEABLE wctSplit #-}

-- | natural transformation which is useful for interoperation between
-- the cache storage and the values returned to the user.
toSem :: Identity a -> P.Sem r a
toSem = pure . runIdentity
{-# INLINE toSem #-}

-- | Access the computation part of a @WithCacheTime a@. This or
-- 'ignoreCacheTimeM' is required to use the cached value as anything but input
-- to another cached computation.
ignoreCacheTime :: WithCacheTime m a -> m a
ignoreCacheTime = actionOnlyQ
{-# INLINEABLE ignoreCacheTime #-}

-- | Access the computation part of an @m (WithCacheTime a)@. This or
-- 'ignoreCacheTime' is required to use the cached value as anything but input
-- to another cached computation.
ignoreCacheTimeM :: Monad m => m (WithCacheTime m a) -> m a
ignoreCacheTimeM = joinActionOnlyQ
{-# INLINEABLE ignoreCacheTimeM #-}

-- | Access the @Maybe Time.UTCTime@ part of a 'WithCacheTime'
cacheTime :: WithCacheTime m a -> TimeM
cacheTime = toTimeM . monoidOnlyQ
{-# INLINEABLE cacheTime #-}

liftActionWithCacheTime :: forall e r a . ActionWithCacheTime r a -> ActionWithCacheTime (e ': r) a
liftActionWithCacheTime = wctApplyNat P.raise

-- | Key/Value store effect requiring its implementation to return values with time-stamps.
data Cache k v m a where
  CacheLookup :: k -> Cache k v m (Maybe (WithCacheTime Identity v))
  CacheUpdate :: k -> Maybe v -> Cache k v m () -- NB: this requires some way to attach a cache time during update
--  CacheLog :: k -> Text -> Cache k v m ()

P.makeSem ''Cache

formatLogMsg :: Show k => k -> Text -> Text
formatLogMsg key msg = "[cache@=" <> show key <> "] " <> msg

cacheLog :: (Show k, K.LogWithPrefixesCat r) => k -> Text -> P.Sem r ()
cacheLog key msg = K.logCat "KH_Cache" K.khDebugLogSeverity (formatLogMsg key msg)

--debugLogSeverity :: K.LogSeverity
--debugLogSeverity  = K.Debug 3
--{-# INLINE debugLogSeverity #-}

-- | Combine the action of serializing and caching
encodeAndStore
  :: ( Show k
     , P.Member (Cache k ct) r
     , K.LogWithPrefixesCat r
     )
  => KS.Serialize CacheError r a ct -- ^ Record-Of-Functions for serialization/deserialization
  -> k                              -- ^ Key
  -> a                              -- ^ Data to encode and cache
  -> P.Sem r ()
encodeAndStore (KS.Serialize encode _ encBytes) k x =
  K.wrapPrefix ("AtomicCache.encodeAndStore (key=" <> show k <> ")") $ do
    cacheLog k  "encoding (serializing) data"
    encoded <- fst <$> encode x
    let nBytes = encBytes encoded
    cacheLog k $ "Storing " <> show nBytes <> " bytes of encoded data in cache"
    cacheUpdate k (Just encoded)
{-# INLINEABLE encodeAndStore #-}

-- | Lookup key and, if that fails, run an action to update the cache.
-- Further, if the item is in cache, but older than time-stamp of the
-- supplied 'ActionWithCacheTime r b', this function calls the given
-- @b -> P.Sem r (Maybe a)@ with the cached value from the supplied
-- 'ActionWithCacheTime m b'.

-- TODO: We need some exception handling here to make sure, in the case of an Atomic cache,
-- the TMVar gets filled somehow and the key deleted from cache.
-- NB: This returns an action with the cache time and another action to get the data.  This allows us
-- to defer deserialization (and maybe loading??) until we actually want to use the data...

-- IDEA: when too old, make new, retrieve old and compare?  If same, use older date? Costs time, but saves downstream rebuilds.
retrieveOrMakeAndUpdateCache
  :: forall ct k r b a.
     ( P.Members [Cache k ct, P.Embed IO] r
     ,  K.LogWithPrefixesCat r
     , K.LogWithPrefixesLE r
     , Show k
     )
  => KS.Serialize CacheError r a ct            -- ^ Record-Of-Functions for serialization/deserialization
  -> (b -> P.Sem r (Maybe a))                  -- ^ Computation to run to make @a@ if cache is empty or expired.
  -> k                                         -- ^ Key
  -> ActionWithCacheTime r b                   -- ^ Cached dependencies of the computation.
  -> P.Sem r (Maybe (ActionWithCacheTime r a)) -- ^ Result of lookup or running computation, wrapped as 'ActionWithCacheTime'. Returns 'Nothing" if lookup fails.
retrieveOrMakeAndUpdateCache (KS.Serialize encode decode encBytes) tryIfMissing key deps =
  K.wrapPrefix ("AtomicCache.retrieveOrMakeAndUpdateCache (key=" <> (show key) <> ")") $ do
    let
      loggedDepsA :: ActionWithCacheTime r b
      loggedDepsA = Q depsCT depsAction where
        Q depsCT depsA = deps
        depsAction = do
          cacheLog key $ "key=" <> show key <> ": Trying to make from given action."
          cacheLog key $ "key=" <> show key <> ": running actions for dependencies."
          depsA
      tryIfMissingACT :: ActionWithCacheTime r (Maybe (ActionWithCacheTime r a))
      tryIfMissingACT = promoteQ tryIfMissing' loggedDepsA where
        tryIfMissing' :: b -> P.Sem r (Maybe (ActionWithCacheTime r a))
        tryIfMissing' b = do
          K.logLE K.Diagnostic $ formatLogMsg key "Out of date or missing. Making new item."
          ma <- tryIfMissing b
          case ma of
            Nothing -> do
              cacheLog key $ "key=" <> show key <> ": Making failed."
              K.logLE K.Error $ "key=" <> show key <> ": Making failed."
              cacheUpdate key Nothing
              return Nothing
            Just a -> do
              cacheLog key $ "key=" <> show key <> ": Buffering/Encoding..."
              (ct', a') <- encode a -- a' is the buffered version of a (if necessary)
              let nBytes = encBytes ct'
              cacheLog key $ "key=" <> show key <> ": serialized to " <> show nBytes <> " bytes."
              cacheLog key "Updating cache..."
              cacheUpdate key (Just ct')
              curTime <- P.embed Time.getCurrentTime -- Should this come from the cache so the times are the same?  Or is it safe enough that this is later?
              cacheLog key "Finished making and updating."
              return $ Just $ withCacheTime (Just curTime) (return a')
    fromCache <- cacheLookup key
    let cacheACT :: ActionWithCacheTime r (Maybe (ActionWithCacheTime r a ))
        cacheACT = case fromCache of
          Just (Q cTimeM mct) -> Q cTimeM cacheACT' where
            cacheACT' :: P.Sem r (Maybe (ActionWithCacheTime r a))
            cacheACT' = do
              let ct = runIdentity mct -- we do this out here only because we want the length.  We could defer this unpacking to the decodeAction
              let nBytes = encBytes ct
              cacheLog key $ "key=" <> show key <> ": Retrieved " <> show nBytes <> " bytes from cache. Decoding..."
              return $ Just $ Q cTimeM (cacheLog key ("Deserializing for key=\"" <> show key <> "\"") >> decode ct)
          Nothing -> Q Nothing $ do
            cacheLog key $ "key=" <> show key <> " running empty cache action.  Which shouldn't happen!"
            K.logLE K.Error $ "key=" <> show key <> " running empty cache action.  Which shouldn't happen!"
            return Nothing
    -- we need this order (try before cache) because empty deps will also have "Nothing" for the time and, if cache empty we should
    -- choose try.  We ignore the outer time because the inner ones carry the time we want to return.
    ignoreCacheTime $ chooseQ tryIfMissingACT cacheACT
{-# INLINEABLE retrieveOrMakeAndUpdateCache #-}

-- | Combine the action of retrieving from cache and deserializing.
-- | Throws if item not found or any other error during retrieval
retrieveAndDecode
  :: forall ct k r a .
     (P.Member (Cache k ct) r
     , P.Member (P.Embed IO) r
     , P.Member (P.Error CacheError) r
     , K.LogWithPrefixesCat r
     , K.LogWithPrefixesLE r
     , Show k
     )
  => KS.Serialize CacheError r a ct    -- ^ Record-Of-Functions for serialization/deserialization
  -> k                                 -- ^ Key
  -> Maybe Time.UTCTime                -- ^ 'Time.UTCTime' which cached data must be newer than.  Use 'Nothing' if any cached data is acceptable.
  -> P.Sem r (ActionWithCacheTime r a) -- ^ Result of lookup or running computation, wrapped as 'ActionWithCacheTime'. Throws 'CacheError' if lookup fails.
retrieveAndDecode s k newestM = K.wrapPrefix ("AtomicCache.retrieveAndDecode (key=" <> show k <> ")") $ do
  fromCache <- retrieveOrMakeAndUpdateCache s (const $ return Nothing) k (onlyCacheTime newestM)
  case fromCache of
    Nothing -> P.throw $ ItemNotFoundError $ "No item found/item too old for key=" <> show k <> "."
    Just x -> return x
{-# INLINEABLE retrieveAndDecode #-}

-- | Combine the action of retrieving from cache and deserializing.
-- | Returns @Nothing@ if item not found, and throws on any other error.
lookupAndDecode
  :: forall ct k r a
   . ( P.Member (Cache k ct) r
     , K.LogWithPrefixesCat r
     , K.LogWithPrefixesLE r
     , P.Member (P.Embed IO) r
     , P.Member (P.Error CacheError) r
     , Show k
     )
  => KS.Serialize CacheError r a ct            -- ^ Record-Of-Functions for serialization/deserialization
  -> k                                         -- ^ Key
  -> Maybe Time.UTCTime                        -- ^ 'Time.UTCTime' which cached data must be newer than.  Use 'Nothing' if any cached data is acceptable.
  -> P.Sem r (Maybe (ActionWithCacheTime r a)) -- ^ Result of lookup or running computation, wrapped as 'ActionWithCacheTime'. Returns 'Nothing" if lookup fails.
lookupAndDecode s k newestM = K.wrapPrefix ("AtomicCache.lookupAndDecode (key=" <> show k <> ")")
                              $ retrieveOrMakeAndUpdateCache s (const $ return Nothing) k (onlyCacheTime newestM)
{-# INLINEABLE lookupAndDecode #-}

-- | Lookup key and, if that fails, run an action to update the cache.
-- Further, if the item is in cache, but older than time-stamp of the
-- supplied 'ActionWithCacheTime r b', this function calls the given
-- @b -> P.Sem r (Maybe a)@ with the cached value from the supplied
-- 'ActionWithCacheTime m b'.
--  Throws if item not found *and* making fails.
retrieveOrMake
  :: forall ct k r a b.
     ( P.Member (Cache k ct) r
     , K.LogWithPrefixesCat r
     , K.LogWithPrefixesLE r
     , P.Member (P.Embed IO) r
     , P.Member (P.Error CacheError) r
     , Show k
     )
  => KS.Serialize CacheError r a ct      -- ^ Record-Of-Functions for serialization/deserialization
  -> k                                   -- ^ Key
  -> ActionWithCacheTime r b             -- ^ Cached Dependencies
  -> (b -> P.Sem r a)                    -- ^ Computation to produce @a@ if lookup fails.
  -> P.Sem r (ActionWithCacheTime r a)   -- ^ Result of lookup or running computation, wrapped as 'ActionWithCacheTime'
retrieveOrMake s key cachedDeps makeAction = K.wrapPrefix ("retrieveOrMake (key=" <> show key <> ")") $ do
  let makeIfMissing x = K.wrapPrefix "retrieveOrMake.makeIfMissing" $ do
        cacheLog key $ "Item (at key=" <> show key <> ") not found/too old. Making..."
        Just <$> makeAction x
  fromCache <- retrieveOrMakeAndUpdateCache s makeIfMissing key cachedDeps
  case fromCache of
    Just x -> return x
    Nothing -> P.throw $ OtherCacheError $ "retrieveOrMake (key =" <> show key <> ") returned with Nothing.  Which should be impossible, unless called with action which produced Nothing."
{-# INLINEABLE retrieveOrMake #-}

-- | Clear the cache at a given key.  Throws an exception if item is not present.
clear :: P.Member (Cache k ct) r => k -> P.Sem r ()
clear k = cacheUpdate k Nothing
{-# INLINEABLE clear #-}

-- | Clear the cache at a given key.  Doesn't throw if item is missing.
clearIfPresent :: (P.Member (Cache k ct) r, P.Member (P.Error CacheError) r) => k -> P.Sem r ()
clearIfPresent k = cacheUpdate k Nothing `P.catch` (\(_ :: CacheError) -> pass)
{-# INLINEABLE clearIfPresent #-}

-- structure for in-memory atomic cache
-- outer TVar so only one thread can get the inner TMVar at a time
-- TMVar so we can block if mulitple threads are trying to read or update
-- the @Maybe@ inside so we can notify waiting threads that whatever they were waiting on
-- to fill the TMVar failed.

-- | Specific type of in-memory cache.
type AtomicMemCache k v = C.TVar (Map k (C.TMVar (Maybe (WithCacheTime Identity v))))

-- | lookup combinator for in-memory AtomicMemCache
atomicMemLookup :: (Ord k
                   , Show k
                   , P.Member (P.Embed IO) r
                   , K.LogWithPrefixesCat r
                   , K.LogWithPrefixesLE r
                   )
                => AtomicMemCache k ct
                -> k
                -> P.Sem r (Maybe (WithCacheTime Identity ct))
atomicMemLookup cache key = K.wrapPrefix "atomicMemLookup" $ do
  cacheLog key "called"
  P.embed $ C.atomically $ do
    mv <- C.readTVar cache >>= fmap join . traverse C.readTMVar . M.lookup key
    case mv of
      Just wctv -> return $ Just wctv
      Nothing -> do
        newTMV <- C.newEmptyTMVar
        C.modifyTVar cache (M.insert key newTMV)
        return Nothing
{-# INLINEABLE atomicMemLookup #-}

-- | data type to simplify logging in AtomicMemCache updates
data MemUpdateAction = Deleted | Replaced | Filled deriving (Show)

-- | update combinator for in-memory AtomicMemCache
atomicMemUpdate :: (Ord k
                   , Show k
                   , P.Member (P.Embed IO) r
                   , K.LogWithPrefixesLE r
                   , K.LogWithPrefixesCat r
                   )
                => AtomicMemCache k ct
                -> k
                -> Maybe ct
                -> P.Sem r ()
atomicMemUpdate cache key mct =
  K.wrapPrefix "atomicMemUpdate" $ do
  let log = cacheLog key
  log "called"
  updateAction <- case mct of
    Nothing -> P.embed (C.atomically $ C.modifyTVar cache (M.delete key)) >> return Deleted
    Just ct -> do
      curTime <- P.embed Time.getCurrentTime
      let wct = withCacheTime (Just curTime) (Identity ct)
      P.embed $ C.atomically $ do
        m <- C.readTVar cache
        case M.lookup key m of
          Nothing -> do
            newTMV <- C.newTMVar (Just wct)
            C.modifyTVar cache (M.insert key newTMV)
            return Filled
          Just tmvM -> do
            wasEmptyTMVar <- C.tryPutTMVar tmvM (Just wct)
            if wasEmptyTMVar
              then return Filled
              else C.swapTMVar tmvM (Just wct) >> return Replaced
  case updateAction of
    Deleted -> log "deleted"
    Replaced -> log "replaced"
    Filled -> log "filled"
{-# INLINEABLE atomicMemUpdate #-}

-- | Interpreter for in-memory only AtomicMemCache
runAtomicInMemoryCache :: (Ord k
                          , Show k
                          , P.Member (P.Embed IO) r
                          , K.LogWithPrefixesLE r
                          , K.LogWithPrefixesCat r
                          )
                       => AtomicMemCache k ct
                       -> P.InterpreterFor (Cache k ct) r
runAtomicInMemoryCache cache =
  P.interpret $ \case
    CacheLookup key -> atomicMemLookup cache key
    CacheUpdate key mct -> atomicMemUpdate cache key mct
{-# INLINEABLE runAtomicInMemoryCache #-}


-- Backed by Another Cache
-- lookup is the hard case.  If we don't find it, we want to check the backup cache
-- and fill in this cache from there, if possible
-- | lookup for an AtomicMemCache which is backed by some other cache, probably a persistence layer.
atomicMemLookupB :: (Ord k
                    , P.Members '[P.Embed IO, Cache k ct] r
                    , K.LogWithPrefixesLE r
                    , K.LogWithPrefixesCat r
                    , Show k
                    )
                 => AtomicMemCache k ct
                 -> k
                 -> P.Sem r (Maybe (WithCacheTime Identity ct))
atomicMemLookupB cache key = K.wrapPrefix "atomicMemLookupB" $ do
  cacheLog key $ "checking in mem cache..."
  x <- P.embed $ C.atomically $ do
    mTMV <- M.lookup key <$> C.readTVar cache
    case mTMV of
      Just tmv -> do
        mv <- C.takeTMVar tmv
        case mv of
          Just wct -> do -- in cache with value (and time)
            C.putTMVar tmv (Just wct)
            return $ Right wct
          Nothing -> return $ Left tmv  -- in cache but set to Nothing
      Nothing -> do -- not found
        newTMV <- C.newEmptyTMVar
        C.modifyTVar cache (M.insert key newTMV)
        return $ Left newTMV
  case x of
    Right wct -> cacheLog key "item found in memory cache" >> return (Just wct)
    Left emptyTMV -> do
      cacheLog key ("cached item not found.  Holding empty TMVar. Checking backup cache...")
      inOtherM <- cacheLookup key
      case inOtherM of
        Nothing -> cacheLog key ("item not found in backup cache.") >> return Nothing
        Just (Q tM mct) -> do
          cacheLog key $ "Item found in backup/persistent cache.  Filling empty TMVar."
          let ct = runIdentity mct
          P.embed $ C.atomically $ C.putTMVar emptyTMV (Just $ WithCacheTime tM (Identity ct))
          cacheLog key "Returning"
          return $ Just $ WithCacheTime tM (pure ct)
{-# INLINEABLE atomicMemLookupB #-}

-- | update for an AtomicMemCache which is backed by some other cache, probably a persistence layer.
-- This just does the update in both caches
atomicMemUpdateB ::  (Ord k, Show k, K.LogWithPrefixesCat r, K.LogWithPrefixesLE r,  P.Members '[P.Embed IO, Cache k ct] r)
                 => AtomicMemCache k ct
                 -> k
                 -> Maybe ct
                 -> P.Sem r ()
atomicMemUpdateB cache key mct = K.wrapPrefix "atomicMemUpdateB" $ do
  cacheLog key "calling atomicMemUpdate"
  atomicMemUpdate cache key mct
  cacheLog key "calling cacheUpdate in backup cache."
  cacheUpdate key mct
{-# INLINEABLE atomicMemUpdateB #-}

-- | interpret Cache via a different-Cache-backed AtomicMemCache
runBackedAtomicInMemoryCache :: (Ord k
                                , Show k
                                , K.LogWithPrefixesLE r
                                , K.LogWithPrefixesCat r
                                , P.Members '[P.Embed IO, Cache k ct] r
                                )
                             => AtomicMemCache k ct
                             -> P.InterpreterFor (Cache k ct) r
runBackedAtomicInMemoryCache cache =
  P.interpret $ \case
    CacheLookup k -> atomicMemLookupB cache k
    CacheUpdate k mct -> atomicMemUpdateB cache k mct
{-# INLINEABLE runBackedAtomicInMemoryCache #-}

-- | re-interpret Cache, using AtomicMemCache for in-memory store, into another cache, usually a persistent store.
backedAtomicInMemoryCache :: (Ord k
                             , Show k
                             , P.Member (P.Embed IO) r
                             , K.LogWithPrefixesLE r
                             , K.LogWithPrefixesCat r
                             )
                          => AtomicMemCache k ct
                          -> P.Sem (Cache k ct ': r) a
                          -> P.Sem (Cache k ct ': r) a
backedAtomicInMemoryCache cache =
  P.reinterpret $ \case
    CacheLookup k -> atomicMemLookupB cache k
    CacheUpdate k mct -> atomicMemUpdateB cache k mct
{-# INLINEABLE backedAtomicInMemoryCache #-}


-- | Interpret Cache via AtomicMemCache and an interpreter for a backing cache,
-- usually a persistence layer.
runPersistenceBackedAtomicInMemoryCache :: (Ord k
                                           , Show k
                                           , P.Member (P.Embed IO) r
                                           , P.Member (P.Error CacheError) r
                                           , K.LogWithPrefixesLE r
                                           , K.LogWithPrefixesCat r
                                           )
                                        => P.InterpreterFor (Cache k ct) r -- persistence layer interpreter
                                        -> AtomicMemCache k ct
                                        -> P.InterpreterFor (Cache k ct) r
runPersistenceBackedAtomicInMemoryCache runPersistentCache cache = runPersistentCache . backedAtomicInMemoryCache cache
{-# INLINEABLE runPersistenceBackedAtomicInMemoryCache #-}

-- | Interpret Cache via AtomicMemCache and an interpreter for a backing cache,
-- usually a persistence layer.  Create a new, empty, AtomicMemCache to begin.
runPersistenceBackedAtomicInMemoryCache' :: (Ord k
                                            , Show k
                                            , P.Member (P.Embed IO) r
                                            , P.Member (P.Error CacheError) r
                                            , K.LogWithPrefixesLE r
                                            , K.LogWithPrefixesCat r
                                            )
                                         => P.InterpreterFor (Cache k ct) r
                                         -> P.InterpreterFor (Cache k ct) r
runPersistenceBackedAtomicInMemoryCache' runPersistentCache x = do
  cache <- P.embed $ C.atomically $ C.newTVar mempty
  runPersistenceBackedAtomicInMemoryCache runPersistentCache cache x
{-# INLINEABLE runPersistenceBackedAtomicInMemoryCache' #-}

-- | Interpreter for Cache via persistence to disk as a Streamly Memory.Array (Contiguous storage of Storables) of Bytes (Word8)
persistStreamlyByteArray
  :: (Show k, P.Member (P.Embed IO) r, P.Member (P.Error CacheError) r, K.LogWithPrefixesLE r, K.LogWithPrefixesCat r)
  => (k -> FilePath)
  -> P.InterpreterFor (Cache k (Streamly.Array.Array Word.Word8)) r
persistStreamlyByteArray keyToFilePath =
  P.interpret $ \case
    CacheLookup k -> K.wrapPrefix "persistAsByteArray.CacheLookup" $ do
      let filePath = keyToFilePath k
#if MIN_VERSION_streamly(0,9,0)
      getContentsWithCacheTime (Streamly.Array.fromStream . Streamly.File.read) filePath
#else
      getContentsWithCacheTime (Streamly.Array.fromStream . Streamly.File.toBytes) filePath
#endif
    CacheUpdate k mct -> K.wrapPrefix "persistAsByteStreamly.CacheUpdate" $ do
      case mct of
        Nothing -> do
           cacheLog k "called with Nothing. Deleting file."
           rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
        Just ct -> do
          cacheLog k "called with content. Writing file."
          let filePath     = keyToFilePath k
              (dirPath, _) = T.breakOnEnd "/" (toText filePath)
          _ <- createDirIfNecessary dirPath
          cacheLog k "Writing serialization to disk."
          cacheLog k $ "Writing " <> show (Streamly.Array.length ct) <> " bytes to disk."
#if MIN_VERSION_streamly(0,8,1)
          rethrowIOErrorAsCacheError $ Streamly.File.putChunk filePath ct
#else
          rethrowIOErrorAsCacheError $ Streamly.File.writeArray filePath ct
#endif
{-# INLINEABLE persistStreamlyByteArray #-}

-- | Interpreter for Cache via persistence to disk as a strict ByteString
persistStrictByteString
  :: (P.Members '[P.Embed IO] r, P.Member (P.Error CacheError) r, K.LogWithPrefixesLE r, K.LogWithPrefixesCat r, Show k)
  => (k -> FilePath)
  -> P.InterpreterFor (Cache k BS.ByteString) r
persistStrictByteString keyToFilePath =
  P.interpret $ \case
    CacheLookup k -> K.wrapPrefix "persistStrictByteString.CacheLookup" $ getContentsWithCacheTime readFileBS (keyToFilePath k)
    CacheUpdate k mct -> K.wrapPrefix "persistStrictByteString.CacheUpdate" $ do
--      let keyText = "key=" <> show k <> ": "
      case mct of
        Nothing -> do
          cacheLog k "called with Nothing. Deleting file."
          rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
        Just ct -> do
          cacheLog k "called with content. Writing file."
          let filePath     = keyToFilePath k
              (dirPath, _) = T.breakOnEnd "/" (toText filePath)
          _ <- createDirIfNecessary dirPath
          cacheLog k "Writing serialization to disk."
          let bsLength = BS.length ct
          cacheLog k $ "Writing " <> show bsLength <> " bytes to disk."
          rethrowIOErrorAsCacheError $ writeFileBS filePath ct  -- maybe we should do this in another thread?
{-# INLINEABLE persistStrictByteString #-}

-- | Interpreter for Cache via persistence to disk as a lazy ByteString
persistLazyByteString
  :: (P.Members '[P.Embed IO] r, P.Member (P.Error CacheError) r, K.LogWithPrefixesLE r,  K.LogWithPrefixesCat r, Show k)
  => (k -> FilePath)
  -> P.InterpreterFor (Cache k BL.ByteString) r
persistLazyByteString keyToFilePath =
  P.interpret $ \case
    CacheLookup k -> K.wrapPrefix "persistAsLazyByteString.CacheLookup" $ getContentsWithCacheTime readFileLBS (keyToFilePath k)
    CacheUpdate k mct -> K.wrapPrefix "persistAsLazyByteString.CacheUpdate" $ do
      case mct of
        Nothing -> do
          cacheLog k "called with Nothing. Deleting file."
          rethrowIOErrorAsCacheError $ System.removeFile (keyToFilePath k)
        Just ct -> do
          cacheLog k "called with content. Writing file."
          let filePath     = keyToFilePath k
              (dirPath, _) = T.breakOnEnd "/" (toText filePath)
          _ <- createDirIfNecessary dirPath
          cacheLog k "Writing serialization to disk."
          let bsLength = BL.length ct
          cacheLog k $ "Writing " <> show bsLength <> " bytes to disk."
          rethrowIOErrorAsCacheError $ writeFileLBS filePath ct  -- maybe we should do this in another thread?
{-# INLINEABLE persistLazyByteString #-}


createDirIfNecessary
  :: (P.Members '[P.Embed IO] r, K.LogWithPrefixesLE r, K.LogWithPrefixesCat r)
  => Text
  -> P.Sem r ()
createDirIfNecessary dir = K.wrapPrefix "createDirIfNecessary" $ do
  K.logCat "KH_Cache" K.khDebugLogSeverity $ "Checking if cache path (\"" <> dir <> "\") exists."
  existsB <- P.embed $ System.doesDirectoryExist (toString dir)
  if existsB then (do
    K.logCat "KH_Cache" K.khDebugLogSeverity $ "\"" <> dir <> "\" exists."
    return ()) else (do
    K.logCat "KH_Cache" K.Info
      $  "Cache directory (\""
      <> dir
      <> "\") not found. Atttempting to create."
    P.embed
      $ System.createDirectoryIfMissing True (toString dir))
{-# INLINEABLE createDirIfNecessary #-}


getContentsWithCacheTime :: (P.Members '[P.Embed IO] r
                            , P.Member (P.Error CacheError) r
                            , K.LogWithPrefixesLE r
                            , K.LogWithPrefixesCat r)
                         => (FilePath -> IO a)
                         -> FilePath
                         -> P.Sem r (Maybe (WithCacheTime Identity a))
getContentsWithCacheTime f fp =  K.wrapPrefix "getContentsWithCacheTime" $ do
  K.logCat "KH_Cache" K.khDebugLogSeverity "Reading serialization from disk."
  rethrowIOErrorAsCacheError $ fileNotFoundToMaybe $ do
    ct <- f fp
    cTime <- System.getModificationTime fp
    return $ withCacheTime (Just cTime) (Identity ct)
{-# INLINE getContentsWithCacheTime #-}

fileNotFoundToEither :: IO a -> IO (Either () a)
fileNotFoundToEither x = fmap Right x `Exception.catch` f where
  f :: Exception.IOException -> IO (Either () a)
  f e = if IO.Error.isDoesNotExistError e then return (Left ()) else Exception.throw e
{-# INLINEABLE fileNotFoundToEither #-}

fileNotFoundToMaybe :: IO a -> IO (Maybe a)
fileNotFoundToMaybe x = fmap Just x `Exception.catch` f where
  f :: Exception.IOException -> IO (Maybe a)
  f e = if IO.Error.isDoesNotExistError e then return Nothing else Exception.throw e
{-# INLINEABLE fileNotFoundToMaybe #-}


rethrowIOErrorAsCacheError :: (P.Member (P.Embed IO) r, P.Member (P.Error CacheError) r) => IO a -> P.Sem r a
rethrowIOErrorAsCacheError = P.fromExceptionVia (\(e :: IO.Error.IOError) -> PersistError $ "IOError: " <> show e)
{-# INLINEABLE rethrowIOErrorAsCacheError #-}
