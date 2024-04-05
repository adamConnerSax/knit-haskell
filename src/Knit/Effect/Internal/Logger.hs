{-# LANGUAGE ConstraintKinds               #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE OverloadedStrings             #-}
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
--    KHLogCategory(..)
--  , khLogCategorySeverity
--  , khLogCategoryText
    khDebugLogSeverity
  , khDebugLog
  )
where

import Knit.Effect.Logger as KLog
import Polysemy as P

khDebugLogSeverity :: KLog.LogSeverity
khDebugLogSeverity =  (KLog.Debug 3)
{-# INLINEABLE khDebugLogSeverity #-}

-- | logging level for debugging messages from the library itself.
khDebugLog :: LogWithPrefixesCat effs => Text -> P.Sem effs ()
khDebugLog = KLog.logCat "KH_Other" khDebugLogSeverity
{-# INLINEABLE khDebugLog #-}
