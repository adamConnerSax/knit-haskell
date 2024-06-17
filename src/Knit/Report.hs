{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Knit.Report
Description : Re-export all the things required for pandoc-built reports.
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module re-exports the basic pieces to build reports using Pandoc.
That is, it is intended as one-stop-shopping for using this library to produce Html from various fragments which
Pandoc can read.

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.

Notes:

1. You can add logging from within document creation using 'logLE'.
2. The "Knit.Report.Input.MarkDown.PandocMarkDown" module is exported
so if you want to use a different markdown flavor you may need to hide "addMarkDown" when you import this module.
3. If you use any other effects in your polysemy stack (e.g., Random or RandomFu), you will need to interpret/run them before calling knitHtml/knitHtmls.
-}
module Knit.Report
  (
    -- * Report Building
    module Knit.Report.EffectStack
  , module Knit.Report.Error

    -- * Inputs
  , module Knit.Report.Input.Table.Colonnade
  , module Knit.Report.Input.MarkDown.PandocMarkDown
  , module Knit.Report.Input.Html
  , module Knit.Report.Input.Html.Blaze
  , module Knit.Report.Input.Html.Lucid
  , module Knit.Report.Input.Latex
  , module Knit.Report.Input.RST
  , module Knit.Report.Input.Visualization.Hvega
  , module Knit.Report.Input.Visualization.Diagrams

    -- * Output
  , module Knit.Report.Output
  , module Knit.Report.Output.Html

    -- * Effects
  , module Polysemy
  , module Knit.Effect.Pandoc
  , module Knit.Effect.Docs
  , module Knit.Effect.PandocMonad
  , module Knit.Effect.Logger
  , module Knit.Effect.UnusedId
  , module Knit.Effect.Serialize
  , module Knit.Effect.Timer

  , module Polysemy.Async
  , module Knit.Report.Cache

  -- * Utilities
  )
where

import           Knit.Report.EffectStack
import           Knit.Report.Error
import           Knit.Report.Cache
import           Knit.Report.Output
import           Knit.Report.Output.Html        ( pandocWriterToBlazeDocument
                                                , mindocOptionsF
                                                , writeAllPandocResultsWithInfoAsHtml
                                                , writePandocResultWithInfoAsHtml
                                                )

import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )
import           Polysemy.Async                 ( async
                                                , await
                                                , sequenceConcurrently
                                                )

import           Knit.Effect.Pandoc             ( ToPandoc
                                                , Requirement(..)
                                                , PandocReadFormat(..)
                                                , PandocWriteFormat(..)
                                                , Pandocs
                                                , PandocInfo(..)
                                                , newPandoc
                                                )
import           Knit.Effect.Docs               ( DocWithInfo(..) )
import           Knit.Effect.PandocMonad
import           Knit.Effect.Logger             ( LogSeverity(..)
                                                , logAll
                                                , nonDiagnostic
                                                , logDebug
                                                , logDiagnostic
                                                , logLE
                                                , logCat
                                                , wrapPrefix
                                                , filteredLogEntriesToIO
                                                , PrefixedLogEffectsLE
                                                , LogWithPrefixesLE
                                                )
import           Knit.Effect.UnusedId           ( getNextUnusedId )
import           Knit.Effect.Serialize          (DefaultCacheData, DefaultSerializer)
import           Knit.Effect.Timer (WithTimer, start, snapshot, finish, timed, withTiming, logWithTiming, logTiming)
import           Knit.Report.Input.Table.Colonnade
import           Knit.Report.Input.MarkDown.PandocMarkDown
                                                ( addMarkDown )
import           Knit.Report.Input.Html         ( addStrictTextHtml
                                                , addLazyTextHtml
                                                )
import           Knit.Report.Input.Html.Blaze   ( addBlaze )
import           Knit.Report.Input.Html.Lucid   ( addLucid )
import           Knit.Report.Input.Latex        ( addLatex )
import           Knit.Report.Input.RST          ( addRST
                                                , addRSTFromFile)
import           Knit.Report.Input.Visualization.Hvega
                                                ( addHvega, addHvega' )
import           Knit.Report.Input.Visualization.Diagrams hiding (snapshot, start)
