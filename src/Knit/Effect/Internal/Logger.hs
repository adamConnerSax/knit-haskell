{-# LANGUAGE ConstraintKinds               #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.Internal.Logger
Description : Types for logging within knit-haskell itself
Copyright   : (c) Adam Conner-Sax 2024
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

-}
module Knit.Effect.Internal.Logger
  (
    KHLogCategory(..)
  , khLogCategorySeverity
  , khLogCategoryText
  , KHLogWithPrefixesCat
  , khDebugLog
  )
where

import Knit.Effect.Logger as KLog
import Polysemy as P

data KHLogCategory = KHCache | KHSerialize | KHOther
  deriving stock (Show, Eq)

khLogCategorySeverity :: KHLogCategory -> KLog.LogSeverity
khLogCategorySeverity _ = KLog.Debug 3
{-# INLINEABLE khLogCategorySeverity #-}

khLogCategoryText :: KHLogCategory -> Text
khLogCategoryText = show
{-# INLINEABLE khLogCategoryText #-}

-- | Constraint helper for Internal @LogCat@ type with prefixes
type KHLogWithPrefixesCat effs = LogWithPrefixesCat KHLogCategory effs

-- | logging level for debugging messages from the library itself.
khDebugLog :: KHLogWithPrefixesCat effs => Text -> P.Sem effs ()
khDebugLog = KLog.logCat KHOther
{-# INLINEABLE khDebugLog #-}
