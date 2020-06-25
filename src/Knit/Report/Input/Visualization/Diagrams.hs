{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-|
Module      : Knit.Report.Input.Visualization.Diagrams
Description : Support addition of Diagrams to knitted reports.
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to Diagrams (from the Diagrams library) to the current Pandoc document.
-}
module Knit.Report.Input.Visualization.Diagrams
  (
    -- * Add Diagrams Inputs
    addDiagramAsSVG
    -- * Diagrams Re-Exports
  , module Diagrams.Prelude
  , module Diagrams.Backend.SVG
  )
where

import           Knit.Report.Input.Html.Blaze   ( addBlaze )
import           Text.Blaze.Html                ( preEscapedLazyText
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as BH
import qualified Text.Blaze.Html5.Attributes   as BHA

import qualified Data.Text                     as T
--import           Data.Maybe                     ( fromMaybe )

import qualified Diagrams.Prelude              as D
import           Diagrams.Prelude         hiding ( trace ) -- this conflicts with Pandoc trace.  TO get it, you'll need to import it directly
--import qualified Diagrams.TwoD.Size            as D
import qualified Diagrams.Backend.SVG          as DSVG
import           Diagrams.Backend.SVG
import qualified Graphics.Svg                  as SVG

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM
import qualified Knit.Effect.UnusedId          as KUI

-- | Add diagram (via SVG inserted as HTML) with user supplied width and height.
addDiagramAsSVG
  :: ( PM.PandocEffects effs
     , P.Member PE.ToPandoc effs
     , P.Member KUI.UnusedId effs
     )
  => Maybe T.Text -- ^ id attribute for figure.  Will use next unused "figure" id if Nothing
  -> Maybe T.Text -- ^ caption for figure
  -> Double -- ^ width in pixels (?)
  -> Double -- ^ height in pixels (?)
  -> D.QDiagram DSVG.SVG D.V2 Double D.Any-- ^ diagram
  -> P.Sem effs T.Text
addDiagramAsSVG idTextM captionTextM wPixels hPixels diagram = do
  idText <- maybe (KUI.getNextUnusedId "figure") return idTextM
  let svgOptions =
        DSVG.SVGOptions (D.dims2D wPixels hPixels) Nothing idText [] False
  addDiagramAsSVGWithOptions (Just idText) captionTextM svgOptions diagram

-- | Add diagram (via SVG inserted as HTML) with user-supplied options.
addDiagramAsSVGWithOptions
  :: ( PM.PandocEffects effs
     , P.Member PE.ToPandoc effs
     , P.Member KUI.UnusedId effs
     )
  => Maybe T.Text -- ^ id attribute for figure, will use next unsed "figure" id if nothing
  -> Maybe T.Text -- ^ caption for figure
  -> DSVG.Options DSVG.SVG D.V2 Double
  -> D.QDiagram DSVG.SVG D.V2 Double D.Any-- ^ diagram
  -> P.Sem effs T.Text
addDiagramAsSVGWithOptions idTextM captionTextM svgOptions diagram = do
  idText <- maybe (KUI.getNextUnusedId "figure") return idTextM
  addBlaze $ BH.figure BH.! BHA.id (toValue idText) $ do
    preEscapedLazyText $ SVG.renderText $ D.renderDia DSVG.SVG
                                                      svgOptions
                                                      diagram
    maybe (return ()) (BH.figcaption . BH.toHtml) captionTextM
  return idText
