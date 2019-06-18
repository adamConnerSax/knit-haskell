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
  , addDiagramAsSVGWithOptions
    -- * re-exports
  , module Diagrams.Prelude
  , module Diagrams.Backend.SVG
  , module Diagrams.Backend.Rasterific
  )
where

import           Knit.Report.Input.MarkDown.PandocMarkDown
                                                ( addMarkDown )
import           Knit.Report.Input.Html.Blaze   ( addBlaze )
import           Text.Blaze.Html                ( preEscapedLazyText
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as BH
import qualified Text.Blaze.Html5.Attributes   as BHA

import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Control.Monad.IO.Class         ( liftIO )
import qualified Diagrams.Prelude              as D
import           Diagrams.Prelude        hiding ( trace ) -- this conflicts with Pandoc trace.  TO get it, you'll need to import it directly

import qualified Diagrams.Backend.Rasterific   as DRAS
import           Diagrams.Backend.Rasterific
                                         hiding ( B )
import qualified Diagrams.Backend.SVG          as DSVG
import           Diagrams.Backend.SVG
import qualified Graphics.Svg                  as SVG

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM
import qualified Knit.Effect.UnusedId          as KUI

-- | Add diagram (via svg inserted as html).
addDiagramAsSVG
  :: (PM.PandocEffects r, P.Member PE.ToPandoc r, P.Member KUI.UnusedId r)
  => Maybe T.Text -- ^ id attribute for figure.  Will use next unused "figure" id if Nothing
  -> Maybe T.Text -- ^ caption for figure
  -> Double -- ^ width in pixels (?)
  -> Double -- ^ height in pixels (?)
  -> D.QDiagram DSVG.SVG D.V2 Double D.Any-- ^ diagram
  -> P.Sem r T.Text
addDiagramAsSVG idTextM captionTextM wPixels hPixels diagram = do
  PE.require PE.DiagramsSVGSupport
  idText <- maybe (KUI.getNextUnusedId "figure") return idTextM
  let svgOptions =
        DSVG.SVGOptions (D.dims2D wPixels hPixels) Nothing idText [] False
  addDiagramAsSVGWithOptions (Just idText) captionTextM svgOptions diagram

addDiagramAsSVGWithOptions
  :: (PM.PandocEffects r, P.Member PE.ToPandoc r, P.Member KUI.UnusedId r)
  => Maybe T.Text -- ^ id attribute for figure, will use next unsed "figure" id if nothing
  -> Maybe T.Text -- ^ caption for figure
  -> DSVG.Options DSVG.SVG D.V2 Double
  -> D.QDiagram DSVG.SVG D.V2 Double D.Any-- ^ diagram
  -> P.Sem r T.Text
addDiagramAsSVGWithOptions idTextM captionTextM svgOptions diagram = do
  PE.require PE.DiagramsSVGSupport
  idText <- maybe (KUI.getNextUnusedId "figure") return idTextM
  addBlaze $ BH.figure BH.! BHA.id (toValue idText) $ do
    preEscapedLazyText $ SVG.renderText $ D.renderDia DSVG.SVG
                                                      svgOptions
                                                      diagram
    maybe (return ()) (BH.figcaption . BH.toHtml) captionTextM
  return idText

-- Not exported!! THis is a WIP and not currently working well.
-- Issues:
-- 1. Path.  Where do we save the file vs. how does html/markdown look for it?
-- right now only absolute paths work
-- 2. Sizing is messed up.  Not sure why
-- | Add diagram (via saving as pdf file and then inserting image ref via pandoc markdown)
addDiagramAsPDF
  :: (PM.PandocEffects r, P.Members '[PE.ToPandoc, KUI.UnusedId, P.Lift IO] r)
  => Maybe T.Text -- ^ optional filename (without extension) for PDF, otherwise will use unique_id.PDF
  -> Maybe T.Text -- ^ caption for figure
  -> Double -- ^ width in pixels (?)
  -> Double -- ^ height in pixels (?)
  -> D.QDiagram DRAS.Rasterific D.V2 Double D.Any-- ^ diagram
  -> P.Sem r ()
addDiagramAsPDF pdfNameM captionTextM wPixels hPixels diagram = do
  pdfPrefix <- maybe (KUI.getNextUnusedId "pdf") return pdfNameM
  let pdfName     = pdfPrefix <> ".pdf"
      sSpec       = mkSizeSpec2D (Just wPixels) (Just hPixels)
      captionText = fromMaybe pdfName captionTextM
      ifNoCaption = maybe ("\\ ") (const "") captionTextM -- a trailing non-breaking space prevents the caption from being included
  liftIO $ DRAS.renderRasterific (T.unpack pdfName) sSpec diagram
  addMarkDown $ "![" <> captionText <> "](" <> pdfName <> ")" <> ifNoCaption
