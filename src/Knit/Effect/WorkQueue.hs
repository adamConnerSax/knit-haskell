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
    -- * Job Type
    Job(..)
  , simpleJob
    
    -- * Effect
  , WorkQueue

    -- * actions    
  , asyncWithQueue
  , sequenceConcurrentlyWithQueue
  
    -- * interpretations
  , runWorkQueue  
  )
where

import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.STM        as STM
import qualified Data.IORef as IORef
import qualified Polysemy                      as P
import qualified Polysemy.Async                as PA
import qualified Polysemy.State          as PS

type WorkQueue = PS.State ((STM.TBQueue (), Int))

data Job r a = Job { job :: P.Sem r a, willUse :: Int }

simpleJob :: P.Sem r a -> Job r a
simpleJob x = Job x 1

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


runWorkQueue :: P.Member (P.Embed IO) r => Int -> P.Sem (WorkQueue ': r) a -> P.Sem r a
runWorkQueue numCapabilities m = do
  initialQ <- P.embed $ STM.atomically $ do
    q <- STM.newTBQueue $ fromIntegral numCapabilities
    return q
  initialStateIORef <- P.embed $ IORef.newIORef (initialQ, numCapabilities)
  PS.runStateIORef initialStateIORef m
{-# INLINEABLE runWorkQueue #-}

  

