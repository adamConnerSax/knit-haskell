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
Description : Effect to manage a simple work queue for asynchronous jobs 
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Work Queue supporting:

1. Allow a specific number of maximum capabilities (CPU cores) to be specified and then used
as they become available.

2. Allow jobs to be submitted with a number indicating how many capabilities they will use when run.

3. Allow jobs with different types to be submitted.
-}
module Knit.Effect.WorkQueue
  (
    -- * Effect
    WorkQueue

    -- * actions    

    -- * interpretations
  )
where

import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.STM        as STM
import qualified Data.IORef as IORef
import qualified Data.Text                     as T
import qualified Polysemy                      as P
import qualified Polysemy.Async                as PA
import qualified Polysemy.State          as PS

type WorkQueue = PS.State ((STM.TBQueue (), Int))

data Job r a = Job { job :: P.Sem r a, willUse :: Int }


asyncWithQueue :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r) => Job r a -> P.Sem r (Async.Async (Maybe a))
asyncWithQueue (Job job n) = do
  (q, numCapabilities) <- PS.get
  capabilitiesToUse <- P.embed $ STM.atomically $ do
--    used <- STM.lengthTBQueue q
    let toUse = max n numCapabilities
    _ <- traverse (STM.writeTBQueue q) $ replicate toUse () -- this blocks until there are enough/all are available
    return toUse
  let jobWithQ = do
        a <- job
        _ <- P.embed
             $ STM.atomically
             $ traverse (\_ -> STM.tryReadTBQueue q) $ replicate capabilitiesToUse () -- try here in case numbers get out of whack
        return a
  PA.async jobWithQ
{-# INLINEABLE asyncWithQueue #-}  

--sequenceConcurrentlyWithQueue :: (P.Member (P.Embed IO) r, P.Member WorkQueue r, P.Member PA.Async r, Traversable t)
--                              => t (P.Sem r a) -> P.Sem r (t (Maybe a))
--sequenceConcurrentlyWithQueue                               


runWorkQueue :: P.Member (P.Embed IO) r => Int -> P.Sem (WorkQueue ': r) a -> P.Sem r a
runWorkQueue numCapabilities m = do
  initialQ <- P.embed $ STM.atomically $ do
    q <- STM.newTBQueue $ fromIntegral numCapabilities
    return q
  initialStateIORef <- P.embed $ IORef.newIORef (initialQ, numCapabilities)
  PS.runStateIORef initialStateIORef m
{-# INLINAEABLE runWorkQueue #-}

  

