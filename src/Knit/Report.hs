{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-|
Module      : Knit.Report
Description : Re-export all the things required for pandoc-built reports.
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module re-exports the basic pieces to build reports using Pandoc.

Notes:

1. The "RandomFu" effect is not imported since the names might clash with Polysemy.Random. Import that effect directly if you need it.
2. Logger functions are imported but assume you will use the 'LogEntry' type.
3. The PandocMarkDown module is exported so if you want to use a different markdown flavor you may need to hide "addMarkDown" when you import this module.
-}
module Knit.Report
  (
  -- * Inputs
    module Knit.Report.Input.MarkDown.PandocMarkDown
  , module Knit.Report.Input.Html
  , module Knit.Report.Input.Html.Blaze
  , module Knit.Report.Input.Html.Lucid
  , module Knit.Report.Input.Latex
  , module Knit.Report.Input.Visualization.Hvega

  -- * Output Formats
  , module Knit.Report.Output.Html

    -- * Effects
  , module Polysemy
  , module Knit.Effects.Pandoc
  , module Knit.Effects.PandocMonad
  , module Knit.Effects.Logger
  )
where


import           Polysemy                       ( Member
                                                , Semantic
                                                )
import Knit.Effects.Pandoc (ToPandoc, Requirement(..), PandocReadFormat (..), PandocWriteFormat(..), Pandocs)
import Knit.Effects.PandocMonad    (Pandoc, PandocEffects, runPandocAndLoggingToIO)
import Knit.Effects.Logger (LogSeverity (..), logAll, nonDiagnostic, logLE, wrapPrefix, filteredLogEntriesToIO, LogWithPrefixesLE)
import Knit.Report.Input.MarkDown.PandocMarkDown (addMarkDown)
import Knit.Report.Input.Html (addStrictTextHtml, addLazyTextHtml)
import Knit.Report.Input.Html.Blaze (addBlaze)
import Knit.Report.Input.Html.Lucid (addLucid)
import Knit.Report.Input.Latex (addLatex)
import Knit.Report.Input.Visualization.Hvega (addHvega)

import Knit.Report.Output.Html (pandocWriterToBlazeDocument, mindocOptionsF)
