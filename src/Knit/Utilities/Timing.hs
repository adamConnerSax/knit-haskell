{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnboxedTuples              #-} -- This is required for the PrimMonad instance
{-# LANGUAGE UndecidableInstances       #-}

module Knit.Utilities.Timing
  (
    cpuTimed
  , withCPUTime
  , logWithTime
  )
where

import Prelude hiding (error)

import qualified Polysemy as P

--import qualified Data.Text as Text
import System.CPUTime (getCPUTime)


-- | Wrap an action with a timer and produce the time with the result
cpuTimed :: P.Member (P.Embed IO) r => P.Sem r a -> P.Sem r (a, Double)
cpuTimed ma = do
  start <- P.embed getCPUTime
  a <- ma
  end <- P.embed getCPUTime
  let diffTime :: Double = fromIntegral(end - start) / 10^(12 :: Int)
  pure $ (a, diffTime)


-- | Build a new action from en existing one and its timing
withCPUTime :: P.Member (P.Embed IO) r => (a -> Double -> P.Sem r b) -> P.Sem r a -> P.Sem r b
withCPUTime withTime ma = cpuTimed ma >>= uncurry withTime

-- | Given a logging function and a way to produce a message from the action result and the time,
-- produce an action which runs that function with that message after the initial action.
logWithTime :: P.Member (P.Embed IO) r => (Text -> P.Sem r ()) -> (a -> Double -> Text) -> P.Sem r a -> P.Sem r a
logWithTime logF logTimeMsg = withCPUTime f where
  f a s = logF (logTimeMsg a s) >> pure a
