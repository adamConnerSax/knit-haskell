{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Knit.Report.Input.Html.Blaze
Description : Support functions for adding Lucid fragments into a report
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to add Blaze fragments into a Pandoc generated report.
-}
module Knit.Report.Input.Html.Blaze
  (
  -- * Add Blaze
    addBlaze
  )
where

import Knit.Report.Input.Html (addLazyTextHtml)
import qualified Text.Blaze.Html               as BH
import qualified Text.Blaze.Html.Renderer.Text as BH

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc           as PE
import qualified Knit.Effect.PandocMonad      as PM


-- | Add Blaze Html 
addBlaze
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => BH.Html
  -> P.Semantic effs ()
addBlaze = addLazyTextHtml . BH.renderHtml
