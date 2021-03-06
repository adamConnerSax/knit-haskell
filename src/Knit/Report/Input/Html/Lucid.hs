{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Knit.Report.Input.Html.Lucid
Description : Support functions for adding Lucid Html fragments into a report
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

import           Knit.Report.Input.Html         ( addLazyTextHtml )
import qualified Lucid                         as LH

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM

-- | Add Lucid Html
addLucid
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => LH.Html ()
  -> P.Sem effs ()
addLucid = addLazyTextHtml . LH.renderText
