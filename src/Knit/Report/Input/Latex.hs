{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Knit.Report.Input.Latex
Description : Support functions for adding LaTeX fragments into a report
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to add LaTeX fragments to the current Pandoc.
-}
module Knit.Report.Input.Latex
  (
    -- * Add LaTeX fragments
    addLatex
  )
where

import qualified Data.Text                     as T
import qualified Text.Pandoc                   as PA

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM

-- | Add LaTeX
addLatex
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> P.Sem effs ()
addLatex lText = do
  PE.require PE.LatexSupport
  PE.addFrom PE.ReadLaTeX PA.def lText
