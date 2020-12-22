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
    , mkAsyncable
    , queueAsyncable
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
--import Data.Functor.Identity (Identity(..))
--import Data.Maybe (fromMaybe)
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
locallyUse Zero (WorkQueueState q nc _) = WorkQueueState q (min 1 nc) Zero
locallyUse (Positive n) (WorkQueueState q nc _) = WorkQueueState q (min 1 (nc - n)) (Positive n)

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
  
data Asyncable r a = Asyncable { asyncableJob :: P.Sem r a, requestedCapabilities ::  Whole }
data Awaitable a = Awaitable { awaitableResult :: Async.Async (Maybe a), reservedCapabilities :: Whole }


{-
First argument holds number of capabilities reserved by this thread once it's running.
Supply @Nothing@ and nothing will be reserved.  Use that for threads which just launch others.
Supply an argument, usually @Just 1@, if the thread will use a full CPU/core while running.
If the thread will launch others which do not themselves use this API, you can reserve n 
threads via @Just n@.
-}
mkAsyncable :: Whole -> P.Sem r a -> Asyncable r a 
mkAsyncable n j = Asyncable j n 

queueAsyncable :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Asyncable r a  -> P.Sem r (Awaitable a)
queueAsyncable (Asyncable action requested) = do
  WorkQueueState q numCapabilities _ <- PS.get
  let iRequested = wholeToInt requested -- 0 here meand this thread runs without reserving a capability.  Used for threads which mostly just spawn others.
      toReserve  = min iRequested (fromIntegral numCapabilities)
  aa <- PA.async $ do
    blockingUse q toReserve
    PS.modify (locallyUse $ intToWhole toReserve) -- this is *local* state so only "action" sees this smaller available set of capabilities
    action
  return $ Awaitable aa (intToWhole toReserve)

queuedAwait :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Awaitable a -> P.Sem r (Maybe a)
queuedAwait (Awaitable aa n) = do
  WorkQueueState q _ _ <- PS.get
  ma <- PA.await aa -- from aa's viewpoint, there are fewer capabilities
  errorThrowingRelease q (wholeToInt n)
  return ma
{-# INLINEABLE queuedAwait #-}

queuedSequenceConcurrently :: (Traversable t
                              , P.Member (P.Embed IO) r
                              , P.Member WorkQueue r
                              , P.Member PA.Async r)
                           => t (Asyncable r a) -> P.Sem r (t (Maybe a))
queuedSequenceConcurrently jobs = traverse queueAsyncable jobs >>= traverse queuedAwait                           
{-# INLINEABLE queuedSequenceConcurrently #-}

simpleQueuedSequenceConcurrently :: (Traversable t
                                    , P.Member (P.Embed IO) r
                                    , P.Member WorkQueue r
                                    , P.Member PA.Async r)
                                 => t (P.Sem r a) -> P.Sem r (t (Maybe a))
simpleQueuedSequenceConcurrently = queuedSequenceConcurrently . fmap (mkAsyncable (Positive 1))
{-# INLINEABLE simpleQueuedSequenceConcurrently #-}

unboundedQueuedSequenceConcurrently :: (Traversable t
                                    , P.Member (P.Embed IO) r
                                    , P.Member WorkQueue r
                                    , P.Member PA.Async r)
                                 => t (P.Sem r a) -> P.Sem r (t (Maybe a))
unboundedQueuedSequenceConcurrently = queuedSequenceConcurrently . fmap (mkAsyncable Zero)
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

  

