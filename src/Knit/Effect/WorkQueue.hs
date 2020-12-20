{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE PolyKinds                     #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE ScopedTypeVariables           #-}
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
    -- * Job Type
    Job(..)
  , simpleJob
    
    -- * Effect
  ,-} WorkQueue

    -- * actions
  , mkQueueableJob  
  , queuedAsync
  , queuedAwait
--  , asyncWithQueue
  , queuedSequenceConcurrently
  , simpleQueuedSequenceConcurrently
  
    -- * interpretations
  , runWorkQueue  
  )
where

import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.STM        as STM
import qualified Data.IORef as IORef
import Numeric.Natural               (Natural)
import qualified Polysemy                      as P
import qualified Polysemy.Async                as PA
import qualified Polysemy.State          as PS

type WorkQueue = PS.State ((STM.TBQueue (), Natural))

data Job r a = Job { job :: P.Sem r a, willUse :: Natural }
data JobResult a = JobResult { jobResult :: Async.Async (Maybe a), used :: Natural }

mkQueueableJob :: (P.Member (P.Embed IO) r, P.Member WorkQueue r) => Natural -> P.Sem r a -> Job r a --P.Sem r a
mkQueueableJob n action =
  let queuedAction = do 
        (q, numCapabilities) <- PS.get
        let toUse = min n numCapabilities
        blockingUse q toUse -- _ <- traverse (STM.writeTBQueue q) $ replicate toUse () -- this blocks until there are enough/all are available
        action
  in Job queuedAction n
{-# INLINEABLE mkQueueableJob #-}

queuedAsync :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Job r a -> P.Sem r (JobResult a)
queuedAsync (Job action n) = do
  aa <- PA.async action
  return $ JobResult aa n
{-# INLINEABLE queuedAsync #-}  

queuedAwait :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => JobResult a -> P.Sem r (Maybe a)
queuedAwait (JobResult aa n) = do
  ma <- PA.await aa
  (q, _) <- PS.get
  errorThrowingRelease q n -- will throw if Q didn't have enough entries to release
  return ma
{-# INLINEABLE queuedAwait #-}


queuedSequenceConcurrently :: (Traversable t
                              , P.Member (P.Embed IO) r
                              , P.Member WorkQueue r
                              , P.Member PA.Async r)
                           => t (Job r a) -> P.Sem r (t (Maybe a))
queuedSequenceConcurrently jobs = traverse queuedAsync jobs >>= traverse queuedAwait                           

simpleQueuedSequenceConcurrently :: (Traversable t
                                    , P.Member (P.Embed IO) r
                                    , P.Member WorkQueue r
                                    , P.Member PA.Async r)
                                 => t (P.Sem r a) -> P.Sem r (t (Maybe a))
simpleQueuedSequenceConcurrently = queuedSequenceConcurrently . fmap (mkQueueableJob 1)
                                    
blockingUse :: P.Member (P.Embed IO) r => STM.TBQueue () -> Natural -> P.Sem r ()
blockingUse q n =  fmap (const ()) $ P.embed $ STM.atomically $ traverse (STM.writeTBQueue q) $ replicate (fromIntegral n) () 
{-# INLINE blockingUse #-}

blockingRelease :: P.Member (P.Embed IO) r => STM.TBQueue () -> Natural -> P.Sem r ()
blockingRelease q n =  fmap (const ()) $ P.embed $ STM.atomically $ traverse (\_ -> STM.readTBQueue q) $ replicate (fromIntegral n) () 
{-# INLINE blockingRelease #-}

nonBlockingRelease :: P.Member (P.Embed IO) r => STM.TBQueue () -> Natural -> P.Sem r ()
nonBlockingRelease q n =  fmap (const ()) $ P.embed $ STM.atomically $ traverse (\_ -> STM.tryReadTBQueue q) $ replicate (fromIntegral n) () 
{-# INLINE nonBlockingRelease #-}

errorThrowingRelease :: P.Member (P.Embed IO) r => STM.TBQueue () -> Natural -> P.Sem r ()
errorThrowingRelease q n = fmap (const ()) $ P.embed $ STM.atomically $ do
  queueLength <- STM.lengthTBQueue q
  if (queueLength < n)
    then error "WorkQueue is trying to release more capabilities than it has reserved."
    else traverse (\_ -> STM.readTBQueue q) $ replicate (fromIntegral n) () 
  

{-
asyncWithQueue :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Job r a -> P.Sem r (Async.Async (Maybe a))
asyncWithQueue (Job j n) = do
  (q, numCapabilities) <- PS.get
--  P.embed $ putStrLn $ "In asyncWithQueue (numCapabilities=" ++ show numCapabilities ++ ")"
--  P.embed $ putStrLn $ "writing () to queue to request use of cores."  
  capabilitiesToUse <- P.embed $ STM.atomically $ do
--    used <- STM.lengthTBQueue q
    let toUse = min n numCapabilities
    _ <- traverse (STM.writeTBQueue q) $ replicate toUse () -- this blocks until there are enough/all are available
    return toUse
--  P.embed $ putStrLn $ "succeeded in writing " ++ show capabilitiesToUse ++ " entries to Q"
  let jobWithQ = do
        a <- j
        _ <- P.embed
             $ STM.atomically             
             $ traverse (\_ -> STM.tryReadTBQueue q) $ replicate capabilitiesToUse () -- "tryRead" here in case numbers get out of whack
--        P.embed $ putStrLn $ "reading " ++ show capabilitiesToUse ++ " entries from queue to release cores."  
        return a
  PA.async jobWithQ
{-# INLINEABLE asyncWithQueue #-}  

sequenceConcurrentlyWithQueue :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r, Traversable t)
                              => t (Job r a) -> P.Sem r (t (Maybe a))
sequenceConcurrentlyWithQueue jobs = traverse asyncWithQueue jobs >>= traverse PA.await                              
{-# INLINEABLE sequenceConcurrentlyWithQueue #-}
-}

runWorkQueue :: P.Member (P.Embed IO) r => Natural -> P.Sem (WorkQueue ': r) a -> P.Sem r a
runWorkQueue numCapabilities m = do
  initialQ <- P.embed $ STM.atomically $ do
    q <- STM.newTBQueue $ fromIntegral numCapabilities
    return q
  initialStateIORef <- P.embed $ IORef.newIORef (initialQ, numCapabilities)
  PS.runStateIORef initialStateIORef m
{-# INLINEABLE runWorkQueue #-}

  

