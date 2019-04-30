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
    Random

    -- * Actions
  , sampleRVar
  , sampleDist

  -- * Interpretations 
  , runRandomIOSimple
  , runRandomIOPureMT
  , runRandomFromSource
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

-- | Random Effect
data Random m r where
  SampleRVar ::  R.RVar t -> Random m t
  GetRandomPrim :: R.Prim t -> Random m t

-- | Convert a random-fu RVar to the Random Effect
sampleRVar :: (P.Member Random effs) => R.RVar t -> P.Sem effs t
sampleRVar = send . SampleRVar

-- | Convert a random-fu Distribution to the Random Effect
sampleDist :: (P.Member Random effs, R.Distribution d t) => d t -> P.Sem effs t
sampleDist = sampleRVar . R.rvar

getRandomPrim :: P.Member Random effs => R.Prim t -> P.Sem effs t
getRandomPrim = send . GetRandomPrim

-- | Run in IO using default random-fu IO source
runRandomIOSimple
  :: forall effs a
   . MonadIO (P.Sem effs)
  => P.Sem (Random ': effs) a
  -> P.Sem effs a
runRandomIOSimple = P.interpret f
 where
  f :: forall m x . (Random m x -> P.Sem effs x)
  f r = case r of
    SampleRVar    rv -> liftIO $ R.sample rv
    GetRandomPrim pt -> liftIO $ R.getRandomPrim pt

-- | Run using the given source
runRandomFromSource
  :: forall s effs a
   . R.RandomSource (P.Sem effs) s
  => s
  -> P.Sem (Random ': effs) a
  -> P.Sem effs a
runRandomFromSource source = P.interpret f
 where
  f :: forall m x . (Random m x -> P.Sem effs x)
  f r = case r of
    SampleRVar    rv -> R.runRVar (R.sample rv) source
    GetRandomPrim pt -> R.runRVar (R.getRandomPrim pt) source

-- | Run in 'IO', using the given 'PureMT' source stored in an 'IORef'
runRandomIOPureMT
  :: MonadIO (P.Sem effs)
  => R.PureMT
  -> P.Sem (Random ': effs) a
  -> P.Sem effs a
runRandomIOPureMT source re =
  liftIO (newIORef source) >>= flip runRandomFromSource re

-- | supply instance of MonadRandom for functions which require it
$(R.monadRandom [d|
        instance P.Member Random effs => R.MonadRandom (P.Sem effs) where
            getRandomPrim = getRandomPrim
    |])
