{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Knit.Report.Input.Html.Lucid
Description : Support functions for adding Lucid fragments into a report
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to add Lucid fragments into a Pandoc generated report.
-}
module Knit.Report.Input.Html.Lucid
  (
  -- * Add Lucid 
    addLucid
  )
where

import Knit.Report.Input.Html (addLazyTextHtml)
import qualified Lucid                         as LH

import qualified Polysemy                      as P
import           Polysemy                       ( Member
                                                , Semantic
                                                )
import qualified Knit.Effects.Pandoc           as PE
import qualified Knit.Effects.PandocMonad      as PM

-- | Add Lucid Html
addLucid
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => LH.Html ()
  -> P.Semantic effs ()
addLucid = addLazyTextHtml . LH.renderText
