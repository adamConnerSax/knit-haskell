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
{-
    -- * Effect
  ,-} WorkQueue

    -- * actions
  , mkAsyncable
  , queuedAsync
  , queuedAwait
  , queuedSequenceConcurrently
  , simpleQueuedSequenceConcurrently
  , unboundedQueuedSequenceConcurrently
  
    -- * interpretations
  , runWorkQueue  
  )
where

import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.STM        as STM
import Data.Maybe (fromMaybe)
import Numeric.Natural               (Natural)
import qualified Polysemy                      as P
import qualified Polysemy.Async                as PA
import qualified Polysemy.State          as PS

data WorkQueueState = WorkQueueState { globalQ :: STM.TBQueue (), localCapabilities :: !Natural }
initialWorkQ :: Natural -> IO WorkQueueState
initialWorkQ n = do
  q <- STM.atomically $ STM.newTBQueue n
  return (WorkQueueState q n)

locallyUse :: Int -> WorkQueueState -> WorkQueueState
locallyUse n (WorkQueueState q nc) = if n > 0
                                     then WorkQueueState q (min 1 (nc - fromIntegral n))
                                     else WorkQueueState q (min 1 nc)

type WorkQueue = PS.State WorkQueueState

data Asyncable r a = Asyncable { asyncableJob :: P.Sem r (Awaitable a) }
data Awaitable a = Awaitable { awaitableResult :: Async.Async (Maybe a), capabilitiesReserved :: Int }

-- First argument holds capabilities for this thread once it's running. Supply @Nothing@ and nothing will be
-- reserved.  Use that for threads which just launch others.
-- Supply an argument, usually @Just 1@, if the thread will use a full CPU/core while running.
mkAsyncable :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Maybe Natural -> P.Sem r a -> Asyncable r a 
mkAsyncable mDesired action = Asyncable $ do
  WorkQueueState q numCapabilities <- PS.get
  let desired :: Int = fromMaybe 0 (fromIntegral <$> mDesired)-- 0 here meand this thread runs without reserving a capability.  Used for threads which mostly just spawn others.
  let toUse  = min desired (fromIntegral numCapabilities)
  aa <- PA.async $ do
    blockingUse q toUse
    PS.modify (locallyUse toUse) -- this is *local* state so only "action" sees this smaller available set of capabilities and what to release
    action
  return $ Awaitable aa toUse
{-# INLINEABLE mkAsyncable #-}

queuedAsync :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Asyncable r a -> P.Sem r (Awaitable a)
queuedAsync = asyncableJob
{-# INLINEABLE queuedAsync #-}  

queuedAwait :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Awaitable a -> P.Sem r (Maybe a)
queuedAwait (Awaitable aa n) = do
  (WorkQueueState q _) <- PS.get
  ma <- PA.await aa -- from aa's viewpoint, there are fewer capabilities
  errorThrowingRelease q n
  return ma
{-# INLINEABLE queuedAwait #-}

queuedSequenceConcurrently :: (Traversable t
                              , P.Member (P.Embed IO) r
                              , P.Member WorkQueue r
                              , P.Member PA.Async r)
                           => t (Asyncable r a) -> P.Sem r (t (Maybe a))
queuedSequenceConcurrently jobs = traverse queuedAsync jobs >>= traverse queuedAwait                           
{-# INLINEABLE queuedSequenceConcurrently #-}

simpleQueuedSequenceConcurrently :: (Traversable t
                                    , P.Member (P.Embed IO) r
                                    , P.Member WorkQueue r
                                    , P.Member PA.Async r)
                                 => t (P.Sem r a) -> P.Sem r (t (Maybe a))
simpleQueuedSequenceConcurrently = queuedSequenceConcurrently . fmap (mkAsyncable (Just 1))
{-# INLINEABLE simpleQueuedSequenceConcurrently #-}

unboundedQueuedSequenceConcurrently :: (Traversable t
                                    , P.Member (P.Embed IO) r
                                    , P.Member WorkQueue r
                                    , P.Member PA.Async r)
                                 => t (P.Sem r a) -> P.Sem r (t (Maybe a))
unboundedQueuedSequenceConcurrently = queuedSequenceConcurrently . fmap (mkAsyncable Nothing)
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
  
-- have to use local state here so the count of available capabilities is local.  Queue of actual capabilities is global
runWorkQueue :: P.Member (P.Embed IO) r => Natural -> P.Sem (WorkQueue ': r) a -> P.Sem r a
runWorkQueue numCapabilities m = do
  initialState <- P.embed $ initialWorkQ numCapabilities
  PS.evalState initialState m
{-# INLINEABLE runWorkQueue #-}

  

