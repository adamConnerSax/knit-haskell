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
module Control.Monad.Freer.Random
  (
    Random
  , sampleRVar
  , sampleDist
--  , Control.Monad.Freer.Random.rvar
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

data Random r where
  SampleRVar ::  R.RVar t -> Random t 
  GetRandomPrim :: R.Prim t -> Random t 

sampleRVar :: (FR.Member Random effs) => R.RVar t -> FR.Eff effs t
sampleRVar = FR.send . SampleRVar

sampleDist :: (FR.Member Random effs, R.Distribution d t) => d t -> FR.Eff effs t
sampleDist = sampleRVar . R.rvar

getRandomPrim :: FR.Member Random effs => R.Prim t -> FR.Eff effs t
getRandomPrim = FR.send . GetRandomPrim

runRandomIOSimple :: forall effs a. MonadIO (FR.Eff effs) => FR.Eff (Random ': effs) a -> FR.Eff effs a
runRandomIOSimple = FR.interpret f where
  f :: forall x. (Random x -> FR.Eff effs x)
  f r = case r of
    SampleRVar rv -> liftIO $ R.sample rv
    GetRandomPrim pt -> liftIO $ R.getRandomPrim pt

runRandomFromSource :: forall s effs a. R.RandomSource (FR.Eff effs) s => s -> FR.Eff (Random ': effs) a -> FR.Eff effs a
runRandomFromSource source = FR.interpret f where
  f :: forall x. (Random x -> FR.Eff effs x)
  f r = case r of
    SampleRVar rv -> R.runRVar (R.sample rv) source 
    GetRandomPrim pt -> R.runRVar (R.getRandomPrim pt) source

runRandomIOPureMT :: MonadIO (FR.Eff effs) => R.PureMT -> FR.Eff (Random ': effs) a -> FR.Eff effs a
runRandomIOPureMT source re = liftIO (newIORef source) >>= flip runRandomFromSource re

$(R.monadRandom [d|
        instance FR.Member Random effs => R.MonadRandom (FR.Eff effs) where
            getRandomPrim = getRandomPrim
    |])

{-
instance FR.Member Random effs => R.MonadRandom (FR.Eff effs) where
  getRandomPrim = getRandomPrim -- this is confusing.  LHS is class member, RHS is function above
-}
