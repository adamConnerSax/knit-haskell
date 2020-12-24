{-# LANGUAGE BangPatterns                  #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE PolyKinds                     #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE TypeApplications              #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.WorkQueue
Description : Wrapper around async for fixed number of CPUs/Cores.
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

-}
module Knit.Effect.WorkQueue
  (
    -- * Effect
     WorkQueue

    -- * actions
    , queueAsyncable
    , queueAsyncableN
    , queuedAwait
    , queuedSequenceConcurrently
    , simpleQueuedSequenceConcurrently
    , unboundedQueuedSequenceConcurrently
    
    -- * interpretations
    , runWorkQueue

    -- * Helper Types
    , Whole(..)
  )
where

import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.STM        as STM
import Numeric.Natural               (Natural)
import qualified Polysemy                      as P
import qualified Polysemy.Async                as PA
import qualified Polysemy.State          as PS

data WorkQueueState = WorkQueueState { globalQ :: STM.TBQueue (), localCapabilities :: !Natural, releaseOnAwait :: Whole }

initialWorkQ :: Natural -> IO WorkQueueState
initialWorkQ n = do
  q <- STM.atomically $ STM.newTBQueue n
  return (WorkQueueState q n Zero)

locallyUse :: Whole -> WorkQueueState -> WorkQueueState
locallyUse Zero (WorkQueueState q nc _) = WorkQueueState q nc Zero -- nothing to free up upon awaiting
locallyUse (Positive n) (WorkQueueState q nc _) = WorkQueueState q nc (Positive n)
--locallyUse (Positive n) (WorkQueueState q nc _) = WorkQueueState q (max 1 (nc - n)) (Positive n)

type WorkQueue = PS.State WorkQueueState

data Whole = Zero | Positive Natural deriving (Show, Eq, Ord)

mapWhole :: (Natural -> Natural) -> Whole -> Whole
mapWhole _ Zero = Zero
mapWhole f (Positive n) = Positive $ f n

wholeToInt :: Whole -> Int
wholeToInt Zero = 0
wholeToInt (Positive n) = fromIntegral n

intToWhole :: Int -> Whole
intToWhole n
  | n >  0 = Positive $ fromIntegral n
  | n == 0 = Zero
  | otherwise = error "negative Int given to intToNumCapabilities."

wholeToNatural :: Whole -> Natural
wholeToNatural Zero = error "Zero is not a Natural number!"
wholeToNatural (Positive n) = n

naturalToWhole :: Natural -> Whole
naturalToWhole n = Positive n
  
newtype Awaitable a = Awaitable { awaitableResult :: Async.Async (Maybe a) }


{- |
Like @async@ but, using an STM queue representing finite CPU resources, this will
block if all allocated capabilitied are busy.
-}
queueAsyncable :: (P.Members [(P.Embed IO), WorkQueue, PA.Async] r) => P.Sem r a  -> P.Sem r (Awaitable a)
queueAsyncable = queueAsyncableN (Positive 1)
{-# INLINEABLE queueAsyncable #-}

{- |
Like @async@ but, using an STM queue representing finite CPU resources, this will
block if all allocated capabilitied are busy.
First argument holds number of capabilities reserved by this thread once it's running.
Supply @Zero@ and nothing will be reserved.  Should be uneccessary since threads queued
this way release held capabilities while awaiting.
Supply an argument, usually @Positive 1@, if the thread will use a full CPU/core while running
(see queueAsyncable).
If the thread will launch others which do not themselves use this API, you can reserve n 
capabilities by supplyign @Positive n@ as the first argument.
-}
queueAsyncableN :: (P.Members [(P.Embed IO), WorkQueue, PA.Async] r) => Whole -> P.Sem r a  -> P.Sem r (Awaitable a)
queueAsyncableN requested action = do
  WorkQueueState q numCapabilities _ <- PS.get
  let iRequested = wholeToInt requested -- 0 here means this thread runs without reserving a capability.  Used for threads which mostly just spawn others.
      toReserve  = min iRequested (fromIntegral numCapabilities)
  aa <- PA.async $ do
    blockingUse q toReserve
    PS.modify (locallyUse $ intToWhole toReserve)
    a <- action
    errorThrowingRelease q toReserve
    return a
  return $ Awaitable aa
{-# INLINEABLE queueAsyncableN #-}

{- |
Await an async computation using the WorkQueue to manage CPU capabilities.
When we await, we free the capabilities used by the awaiting thread.  And when the thread we await
finishes, we reclaim those resources.
-}
queuedAwait :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Awaitable a -> P.Sem r (Maybe a)
queuedAwait (Awaitable aa) = do
  WorkQueueState q _ wReleaseOnAwait <- PS.get
  errorThrowingRelease q (wholeToInt wReleaseOnAwait) 
  ma <- PA.await aa
  blockingUse q (wholeToInt wReleaseOnAwait)
  return ma
{-# INLINEABLE queuedAwait #-}

{- |
A @sequenceConcurrently@ equivalent using the WorkQueue.
-}
queuedSequenceConcurrently :: (Traversable t
                              , P.Member (P.Embed IO) r
                              , P.Member WorkQueue r
                              , P.Member PA.Async r)
                           => t (Whole, P.Sem r a) -> P.Sem r (t (Maybe a))
queuedSequenceConcurrently jobs = traverse (uncurry queueAsyncableN) jobs >>= traverse queuedAwait                           
{-# INLINEABLE queuedSequenceConcurrently #-}

{- |
A @sequenceConcurrently@ equivalent using the WorkQueue.  Reserves one capability per thread.
-}
simpleQueuedSequenceConcurrently :: (Traversable t
                                    , P.Member (P.Embed IO) r
                                    , P.Member WorkQueue r
                                    , P.Member PA.Async r)
                                 => t (P.Sem r a) -> P.Sem r (t (Maybe a))
simpleQueuedSequenceConcurrently jobs = traverse queueAsyncable jobs >>= traverse queuedAwait                           
{-# INLINEABLE simpleQueuedSequenceConcurrently #-}

{- |
A @sequenceConcurrently@ equivalent using the WorkQueue.  Reserves zero capability per thread.
-}
unboundedQueuedSequenceConcurrently :: (Traversable t
                                    , P.Member (P.Embed IO) r
                                    , P.Member WorkQueue r
                                    , P.Member PA.Async r)
                                 => t (P.Sem r a) -> P.Sem r (t (Maybe a))
unboundedQueuedSequenceConcurrently = queuedSequenceConcurrently . fmap (\j -> (Zero, j))
{-# INLINEABLE unboundedQueuedSequenceConcurrently #-}


                                   
blockingUse :: (P.Member (P.Embed IO) r, P.Member WorkQueue r) => STM.TBQueue () -> Int -> P.Sem r ()
blockingUse q n =  do
  fmap (const ()) $ P.embed $ STM.atomically $ traverse (STM.writeTBQueue q) $ replicate n ()
{-# INLINE blockingUse #-}

blockingRelease :: P.Member (P.Embed IO) r => STM.TBQueue () -> Int -> P.Sem r ()
blockingRelease q n =  fmap (const ()) $ P.embed $ STM.atomically $ traverse (\_ -> STM.readTBQueue q) $ replicate n () 
{-# INLINE blockingRelease #-}

nonBlockingRelease :: P.Member (P.Embed IO) r => STM.TBQueue () -> Int -> P.Sem r ()
nonBlockingRelease q n =  fmap (const ()) $ P.embed $ STM.atomically $ traverse (\_ -> STM.tryReadTBQueue q) $ replicate n () 
{-# INLINE nonBlockingRelease #-}

errorThrowingRelease :: P.Member (P.Embed IO) r => STM.TBQueue () -> Int -> P.Sem r ()
errorThrowingRelease q n = fmap (const ()) $ P.embed $ STM.atomically $ do
  queueLength <- STM.lengthTBQueue q
  if (queueLength < fromIntegral n)
    then error "WorkQueue is trying to release more capabilities than it has reserved."
    else traverse (\_ -> STM.readTBQueue q) $ replicate n () 
  
-- state is *thread-local* here so the count of available capabilities is thread-local.
-- Queue of actual capabilities is global since it's made of TVars.
runWorkQueue :: P.Member (P.Embed IO) r => Natural -> P.Sem (WorkQueue ': r) a -> P.Sem r a
runWorkQueue numCapabilities m = do
  initialState <- P.embed $ initialWorkQ numCapabilities
  PS.evalState initialState m
{-# INLINEABLE runWorkQueue #-}

  

