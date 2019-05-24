{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE RankNTypes                    #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE LambdaCase                    #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE PolyKinds                     #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE InstanceSigs                  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.RandomFu
Description : Polysemy random-fu effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Polysemy "random-fu" effect.
Allows a polysemy "stack" to satisfy a MonadRandom (from "random-fu") constraint.
This can be run in a few ways:

1. Directly in 'IO'
2. Using any 'Data.Random.RandomSource' from "random-fu"
3. In 'IO', using a given 'Data.Random.Source.PureMT' source. ('IO' is used to put the source in an 'IORef')
-}
module Knit.Effect.RandomFu
  (
    -- * Effect
    RandomFu

    -- * Actions
  , sampleRVar
  , sampleDist

  -- * Interpretations 
  , runRandomFuIOSimple
  , runRandomFuIOPureMT
  , runRandomFuFromSource

  -- * Interop  
  , absorbMonadRandom

  -- * Deprecated, will be removed in next release
  , Random
  )
where

import qualified Polysemy                      as P
import           Polysemy.Internal              ( send )

import           Data.IORef                     ( newIORef )
import qualified Data.Random                   as R
import qualified Data.Random.Source            as R
import qualified Data.Random.Internal.Source   as R
import qualified Data.Random.Source.PureMT     as R

import           Control.Monad.IO.Class         ( MonadIO(..) )

--import           Data.Kind                      ( Constraint )

-- | Random Effect
data RandomFu m r where
  SampleRVar ::  R.RVar t -> RandomFu m t
  GetRandomPrim :: R.Prim t -> RandomFu m t

type Random = RandomFu
{-# DEPRECATED Random "Use RandomFu instead" #-}

-- | Convert a random-fu RVar to the Random Effect
sampleRVar :: (P.Member RandomFu effs) => R.RVar t -> P.Sem effs t
sampleRVar = send . SampleRVar

-- | Convert a random-fu Distribution to the Random Effect
sampleDist
  :: (P.Member RandomFu effs, R.Distribution d t) => d t -> P.Sem effs t
sampleDist = sampleRVar . R.rvar

getRandomPrim :: P.Member RandomFu effs => R.Prim t -> P.Sem effs t
getRandomPrim = send . GetRandomPrim

-- | Run in IO using default random-fu IO source
runRandomFuIOSimple
  :: forall effs a
   . MonadIO (P.Sem effs)
  => P.Sem (RandomFu ': effs) a
  -> P.Sem effs a
runRandomFuIOSimple = P.interpret f
 where
  f :: forall m x . (RandomFu m x -> P.Sem effs x)
  f r = case r of
    SampleRVar    rv -> liftIO $ R.sample rv
    GetRandomPrim pt -> liftIO $ R.getRandomPrim pt

-- | Run using the given source
runRandomFuFromSource
  :: forall s effs a
   . R.RandomSource (P.Sem effs) s
  => s
  -> P.Sem (RandomFu ': effs) a
  -> P.Sem effs a
runRandomFuFromSource source = P.interpret f
 where
  f :: forall m x . (RandomFu m x -> P.Sem effs x)
  f r = case r of
    SampleRVar    rv -> R.runRVar (R.sample rv) source
    GetRandomPrim pt -> R.runRVar (R.getRandomPrim pt) source

-- | Run in 'IO', using the given 'PureMT' source stored in an 'IORef'
runRandomFuIOPureMT
  :: MonadIO (P.Sem effs)
  => R.PureMT
  -> P.Sem (RandomFu ': effs) a
  -> P.Sem effs a
runRandomFuIOPureMT source re =
  liftIO (newIORef source) >>= flip runRandomFuFromSource re

newtype RandomFuSem r a = RandomFuSem { unRandomFuSem :: P.Sem r a } deriving (Functor, Applicative, Monad)

$(R.monadRandom [d|
        instance P.Member RandomFu r => R.MonadRandom (RandomFuSem r) where
            getRandomPrim = RandomFuSem . getRandomPrim
    |])

{- | Given a function that uses the random-fu package and produces a result thusly
constrained by 'MonadRandom', absorb it into a Polysemy monad whose
effect list contains the RandomFu effect.
-}
absorbMonadRandom
  :: P.Member RandomFu r => (forall m . R.MonadRandom m => m a) -> P.Sem r a
absorbMonadRandom = unRandomFuSem

