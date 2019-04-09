{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Control.Monad.Freer.Random
Description : freer-simple random effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

freer-simple Random effect.  Allows a freer-simple stack to satisfy a MonadRandom (from random-fu) constraint.  This can be run in a few ways:
1. In a simple way in @IO@
2. Using any Data.Random.RandomSource (random-fu)
3. In IO, using a given Data.Random.Source.PureMT source.  (IO is used to put the source in an IORef)
4. (TODO) Reinterpreted in a State (PureMT) effect.
-}
module Control.Monad.Freer.Random
  (
    -- * Types 
    Random
    -- * combinators
  , sampleRVar
  , sampleDist
  -- * run functions 
  , runRandomIOSimple
  , runRandomIOPureMT
  , runRandomFromSource
  ) where
import           Data.IORef (newIORef)
import qualified Data.Random as R
import qualified Data.Random.Source as R
import qualified Data.Random.Internal.Source as R
import qualified Data.Random.Source.PureMT  as R
import qualified Control.Monad.Freer         as FR

import           Control.Monad.IO.Class (MonadIO(..))

-- | Random Effect
data Random r where
  SampleRVar ::  R.RVar t -> Random t 
  GetRandomPrim :: R.Prim t -> Random t 

-- | Convert a random-fu RVar to the Random Effect
sampleRVar :: (FR.Member Random effs) => R.RVar t -> FR.Eff effs t
sampleRVar = FR.send . SampleRVar

-- | Convert a random-fu Distribution to the Random Effect
sampleDist :: (FR.Member Random effs, R.Distribution d t) => d t -> FR.Eff effs t
sampleDist = sampleRVar . R.rvar

getRandomPrim :: FR.Member Random effs => R.Prim t -> FR.Eff effs t
getRandomPrim = FR.send . GetRandomPrim

-- | Run in IO using default random-fu IO source
runRandomIOSimple :: forall effs a. MonadIO (FR.Eff effs) => FR.Eff (Random ': effs) a -> FR.Eff effs a
runRandomIOSimple = FR.interpret f where
  f :: forall x. (Random x -> FR.Eff effs x)
  f r = case r of
    SampleRVar rv -> liftIO $ R.sample rv
    GetRandomPrim pt -> liftIO $ R.getRandomPrim pt

-- | run using the given source
runRandomFromSource :: forall s effs a. R.RandomSource (FR.Eff effs) s => s -> FR.Eff (Random ': effs) a -> FR.Eff effs a
runRandomFromSource source = FR.interpret f where
  f :: forall x. (Random x -> FR.Eff effs x)
  f r = case r of
    SampleRVar rv -> R.runRVar (R.sample rv) source 
    GetRandomPrim pt -> R.runRVar (R.getRandomPrim pt) source

-- | run in IO, using the given PureMT source and IO to store in IORef
runRandomIOPureMT :: MonadIO (FR.Eff effs) => R.PureMT -> FR.Eff (Random ': effs) a -> FR.Eff effs a
runRandomIOPureMT source re = liftIO (newIORef source) >>= flip runRandomFromSource re

-- | supply insstance of MonadRandom for functions which require it
$(R.monadRandom [d|
        instance FR.Member Random effs => R.MonadRandom (FR.Eff effs) where
            getRandomPrim = getRandomPrim
    |])

{-
instance FR.Member Random effs => R.MonadRandom (FR.Eff effs) where
  getRandomPrim = getRandomPrim -- this is confusing.  LHS is class member, RHS is function above
-}
