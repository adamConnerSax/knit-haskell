{-# LANGUAGE ConstraintKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE PolyKinds                  #-}

module Knit.Effect.Timer
  (
    Timer
  , WithTimer
  , start
  , snapshot
  , finish
  , timed
  , withTiming
  , logWithTiming
  , logTiming
  , interpretTimerInIO
  )
where

import Prelude hiding (error)

import Knit.Effect.UnusedId as KU

import qualified Polysemy as P
import qualified Polysemy.AtomicState as P
import           Polysemy.Internal ( send )
import qualified Data.IORef                    as IORef
import qualified Data.Map.Strict as Map

import Text.Printf (printf)

--import qualified Data.Text as Text
import System.CPUTime (getCPUTime)
import qualified Data.Time.Clock as T

data Timings a b = Timings { wall :: a, cpu :: b }

type Timed = Timings Double Double

type Snapshot = Timings T.UTCTime Integer

snapsTimed :: Snapshot -> Snapshot -> Timed
snapsTimed (Timings utcStart cpuStart) (Timings utcEnd cpuEnd) = Timings (utcDiffSecs utcEnd utcStart) (cpuDiffSecs cpuEnd cpuStart)
  where
    cpuDiffSecs s e = fromIntegral (e - s) / 10 ^ (12 :: Int)
    utcDiffSecs s e = realToFrac $ T.nominalDiffTimeToSeconds $ T.diffUTCTime e s

data Timer k m r where
  Start    :: k -> Timer k m ()
  Snapshot :: k -> Timer k m (Maybe Timed)
  Finish   :: k -> Timer k m (Maybe Timed)

start :: P.Member (Timer k) effs => k -> P.Sem effs ()
start = send . Start
{-# INLINEABLE start #-}

snapshot :: P.Member (Timer k) effs => k -> P.Sem effs (Maybe Timed)
snapshot = send . Snapshot
{-# INLINEABLE snapshot #-}

finish :: P.Member (Timer k) effs => k -> P.Sem effs (Maybe Timed)
finish = send . Finish
{-# INLINEABLE finish #-}

type TimerStartMap k = P.AtomicState (Map.Map k Snapshot)

getSnap :: P.Member (P.Embed IO) effs => P.Sem effs Snapshot
getSnap = Timings <$>  P.embed T.getCurrentTime <*> P.embed getCPUTime
{-# INLINEABLE getSnap #-}

interpretTimerInIO :: forall k effs . (Ord k, P.Member (P.Embed IO) effs) => P.InterpreterFor (Timer k) effs
interpretTimerInIO mx = do
  ioRef <- P.embed $ IORef.newIORef mempty
  let nat :: (forall ri x . (Timer k) (P.Sem ri) x -> P.Sem (TimerStartMap k ': effs) x)
      nat = \case
        Start k -> do
          snap <- getSnap
          P.atomicModify $ Map.insert k snap
        Snapshot k -> do
          startTimeM <- Map.lookup k <$> P.atomicGet
          case startTimeM of
            Nothing -> pure Nothing
            Just startTime -> do
              snap <- getSnap
              pure $ Just $ snapsTimed snap startTime
        Finish k -> do
          startTimeM <- Map.lookup k <$> P.atomicGet
          case startTimeM of
            Nothing -> pure Nothing
            Just startTime -> do
              snap <- getSnap
              P.atomicModify @(Map k Snapshot) $ Map.delete k
              pure $ Just $ snapsTimed snap startTime
  P.runAtomicStateIORef ioRef
    $ P.reinterpret nat mx

type WithTimer r = (P.Members [Timer Text, KU.UnusedId] r)

-- | Wrap an action with a timer and produce the time with the result
timed :: WithTimer r => P.Sem r a -> P.Sem r (a, Maybe Timed)
timed ma = do
  timerId <- KU.getNextUnusedId "AnonTimer"
  start timerId
  a <- ma
  tM <- finish timerId
  pure (a, tM)
{-# INLINEABLE timed #-}

-- | Build a new action from en existing one and its timing
withTiming :: WithTimer r => (a -> Maybe Timed -> P.Sem r b) -> P.Sem r a -> P.Sem r b
withTiming withTime ma = timed ma >>= uncurry withTime
{-# INLINEABLE withTiming #-}

-- | Given a logging function and a way to produce a message from the action result and the time,
-- produce an action which runs that function with that message after the initial action.
logWithTiming :: WithTimer r => (Text -> P.Sem r ()) -> (a -> Maybe Timed -> Text) -> P.Sem r a -> P.Sem r a
logWithTiming logF logTimeMsg = withTiming f where
  f a s = logF (logTimeMsg a s) >> pure a
{-# INLINEABLE logWithTiming #-}

-- | Given a logging function and a way to produce a message from the action result and the time,
-- produce an action which runs that function with that message after the initial action.
logTiming :: WithTimer r => (Text -> P.Sem r ()) -> Text -> P.Sem r a -> P.Sem r a
logTiming logF t ma = logF (t <> "...") >> logWithTiming logF msg ma where
  msg _ tM = case tM of
    Just (Timings wallS cpuS) -> t <> " [wall: " <> toText @String (printf "%0.3f" wallS) <> "s; cpu: " <> toText @String (printf "%0.3f" cpuS) <> "]"
    Nothing -> "(TIMING ERROR)"
{-# INLINEABLE logTiming #-}
