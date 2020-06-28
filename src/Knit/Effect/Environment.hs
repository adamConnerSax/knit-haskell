{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE PolyKinds                     #-}
{-# LANGUAGE RankNTypes                    #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE TypeApplications              #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.Environment
Description : Wrapper around Polysemy.Reader for making some functions available to
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

-}
module Knit.Effect.Environment
  (
    -- * Environment
    KnitEnvironment
    -- * Effect
  , KnitEnv
    -- * actions    
  , getLogWithPrefixIO
  , getSerializeDict

    -- * interpretations
  , runEnvReader
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.Reader               as PR

import qualified Knit.Effect.Logger            as KLog
import qualified Knit.Effect.Serialize         as KS


-- | Structure to hold things we want available (via Reader) to any function that needs them.
data KnitEnvironment c ct =
  KnitEnvironment
  {
    keLogWithPrefixIO :: KLog.LogWithPrefixIO
  , keSerializeDict :: KS.SerializeDict c ct
  }
  
type KnitEnv c ct = PR.Reader (KnitEnvironment c ct)   

getLogWithPrefixIO :: P.Member (KnitEnv c ct) r => P.Sem r (KLog.LogWithPrefixIO)
getLogWithPrefixIO = PR.asks keLogWithPrefixIO

getSerializeDict :: P.Member (KnitEnv c ct) r => P.Sem r (KS.SerializeDict c ct)
getSerializeDict = PR.asks keSerializeDict


-- | Run the EnvReader effect
runEnvReader :: KnitEnvironment c ct -> P.InterpreterFor (KnitEnv c ct) r
runEnvReader = PR.runReader
