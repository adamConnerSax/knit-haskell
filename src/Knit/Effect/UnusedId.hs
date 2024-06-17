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
Module      : Knit.Effect.UnusedId
Description : Wrapper around Polysemy.State for generating unused ids with a given prefix
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module contains a state effect used to maintain a map of unused id numbers which can be used for HTML
ids or figure numbering.

-}
module Knit.Effect.UnusedId
  (
    -- * Effect
    UnusedId

    -- * actions
  , getNextUnusedId

    -- * interpretations
  , runUnusedId
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.AtomicState                as PS

import qualified Data.Map                      as M
import qualified Data.Text                     as T

import qualified Data.IORef                    as IORef

-- | Type alias for the dictionary ('M.Map') of current last used id at each prefix.
type IdMap = M.Map T.Text Int

-- | Type alias for 'Polysemy.AtomicState' using "IdMap".
type UnusedId = PS.AtomicState IdMap

-- | Get an unused id with prefix as specified.  Useful for figures, etc.
getNextUnusedId :: P.Member UnusedId r => T.Text -> P.Sem r T.Text
getNextUnusedId prefixT = do
  idMap <- PS.atomicGet @IdMap
  let nextId = fromMaybe 1 $ M.lookup prefixT idMap
  PS.atomicPut $ M.insert prefixT (nextId + 1) idMap
  return $ prefixT <> "_" <> show nextId

-- | Run the UnusedId effect and throw away the state.
runUnusedId :: P.Member (P.Embed IO) r => P.Sem (UnusedId ': r) a -> P.Sem r a
runUnusedId ma = do
  ioRef <- P.embed $ IORef.newIORef mempty
  PS.runAtomicStateIORef ioRef ma
