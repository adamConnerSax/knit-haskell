{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE PolyKinds                  #-}

module Knit.Effect.Timer
  (
    timed
  , withTiming
  , logWithTiming
  , logTiming
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

data Timer k m r where
  Start    :: k -> Timer k m ()
  Snapshot :: k -> Timer k m (Maybe Double)
  Finish   :: k -> Timer k m (Maybe Double)

start :: P.Member (Timer k) effs => k -> P.Sem effs ()
start = send . Start
{-# INLINEABLE start #-}

snapshot :: P.Member (Timer k) effs => k -> P.Sem effs (Maybe Double)
snapshot = send . Snapshot
{-# INLINEABLE snapshot #-}

finish :: P.Member (Timer k) effs => k -> P.Sem effs (Maybe Double)
finish = send . Finish
{-# INLINEABLE finish #-}

type TimerStartMap k = P.AtomicState (Map.Map k Integer)

interpretTimerInIO :: forall k effs . (Ord k, P.Member (P.Embed IO) effs) => P.InterpreterFor (Timer k) effs
interpretTimerInIO mx = do
  ioRef <- P.embed $ IORef.newIORef mempty
  let nat :: (forall ri x . (Timer k) (P.Sem ri) x -> P.Sem (TimerStartMap k ': effs) x)
      nat = \case
        Start k -> do
          startTime <- P.embed getCPUTime
          P.atomicModify $ Map.insert k startTime
        Snapshot k -> do
          startTimeM <- Map.lookup k <$> P.atomicGet
          case startTimeM of
            Nothing -> pure Nothing
            Just startTime -> do
              snapTime <- P.embed getCPUTime
              pure $ Just $ fromIntegral (snapTime - startTime) / 10^(12 :: Int)
        Finish k -> do
          startTimeM <- Map.lookup k <$> P.atomicGet
          case startTimeM of
            Nothing -> pure Nothing
            Just startTime -> do
              endTime <- P.embed getCPUTime
              P.atomicModify @(Map k Integer) $ Map.delete k
              pure $ Just $ fromIntegral (endTime - startTime) / 10^(12 :: Int)
  P.runAtomicStateIORef ioRef
    $ P.reinterpret nat mx

-- | Wrap an action with a timer and produce the time with the result
timed :: P.Members [Timer Text, KU.UnusedId] r => P.Sem r a -> P.Sem r (a, Maybe Double)
timed ma = do
  timerId <- KU.getNextUnusedId "AnonTimer"
  start timerId
  a <- ma
  tM <- finish timerId
  pure (a, tM)
{-# INLINEABLE timed #-}

-- | Build a new action from en existing one and its timing
withTiming :: P.Members [Timer Text, KU.UnusedId] r => (a -> Maybe Double -> P.Sem r b) -> P.Sem r a -> P.Sem r b
withTiming withTime ma = timed ma >>= uncurry withTime
{-# INLINEABLE withTiming #-}

-- | Given a logging function and a way to produce a message from the action result and the time,
-- produce an action which runs that function with that message after the initial action.
logWithTiming :: P.Members [Timer Text, KU.UnusedId] r => (Text -> P.Sem r ()) -> (a -> Maybe Double -> Text) -> P.Sem r a -> P.Sem r a
logWithTiming logF logTimeMsg = withTiming f where
  f a s = logF (logTimeMsg a s) >> pure a
{-# INLINEABLE logWithTiming #-}

-- | Given a logging function and a way to produce a message from the action result and the time,
-- produce an action which runs that function with that message after the initial action.
logTiming :: P.Members [Timer Text, KU.UnusedId] r => (Text -> P.Sem r ()) -> Text -> P.Sem r a -> P.Sem r a
logTiming logF t ma = logF (t <> "...") >> logWithTiming logF g ma where
  g _ tM = case tM of
    Just s -> "took " <> toText @String (printf "%0.3f" s) <> "s"
    Nothing -> "(TIMING ERROR)"
{-# INLINEABLE logTiming #-}
