{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Knit.Report.Input.Latex
Description : Support functions for adding LaTeX fragments into a report
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to add table fragments, using <https://hackage.haskell.org/package/colonnade Colonnade>, to the current Pandoc.

Notes:

1. The way these tables render will depend entirely on how styling is set in the pandoc template.  For Html, this will be determined by css in the template.
Styling put in here does not make it through to Html output.
If you need specific styling, you can use the same colonnade functions these do and embed raw html. Somehow.
-}
module Knit.Report.Input.Table.Colonnade
  (
    -- * Add Colonnade table fragments
    addColonnadeTextTable
  , addColonnadeHtmlTable
  , addColonnadeCellTable
  -- * Re-exports
  , module Colonnade
  , module Text.Blaze.Colonnade
  )
where

import qualified Colonnade                     as C
import           Colonnade
import qualified Text.Blaze.Colonnade          as BC
import           Text.Blaze.Colonnade
import qualified Text.Blaze.Html               as BH
import qualified Text.Blaze.Html5.Attributes   as BHA
import           Knit.Report.Input.Html.Blaze   ( addBlaze )

import           Data.Text                      ( Text )
import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM

-- | Add a table given a Colonnade representation producing text 
addColonnadeTextTable
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs, Foldable f)
  => C.Colonnade C.Headed a Text -- ^ How to encode data as columns
  -> f a -- ^ collection of data
  -> P.Sem effs ()
addColonnadeTextTable colonnade rows = do
  let toCell t = BC.Cell (BHA.style "border: 1px solid black") (BH.toHtml t) -- styling here gets lost.  But maybe I can fix?
  addBlaze $ BC.encodeCellTable
    (BHA.style "border: 1px solid black; border-collapse: collapse") -- this gets lost.  Leaving it here in case I fix that!
    (fmap toCell colonnade)
    rows

-- | Add a <https://hackage.haskell.org/package/blaze-colonnade Blaze-Colonnade> Html Table
addColonnadeHtmlTable
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs, Foldable f)
  => BH.Attribute -- ^ Attributes of <table> Html element, currently unused by knit-haskell
  -> C.Colonnade C.Headed a BH.Html -- ^ How to encode data as columns
  -> f a -- ^ collection of data
  -> P.Sem effs ()
addColonnadeHtmlTable attr colonnade rows =
  addBlaze $ BC.encodeHtmlTable attr colonnade rows

-- | Add a <https://hackage.haskell.org/package/blaze-colonnade Blaze-Colonnade> Cell Table
addColonnadeCellTable
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs, Foldable f)
  => BH.Attribute -- ^ Attributes of <table> Html element, currently unused by knit-haskell
  -> C.Colonnade C.Headed a BC.Cell -- ^ How to encode data as columns
  -> f a -- ^ collection of data
  -> P.Sem effs ()
addColonnadeCellTable attr colonnade rows =
  addBlaze $ BC.encodeCellTable attr colonnade rows
