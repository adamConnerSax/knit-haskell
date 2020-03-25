{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Knit.Kernmantle.Report
  (
    module Knit.Kernmantle
  , module Knit.Kernmantle.Report
  ) where

import Knit.Kernmantle
import qualified Knit.Report as K

import qualified Knit.Effect.Pandoc       as PE
import qualified Knit.Effect.UnusedId                      as KUI
import qualified Knit.Report.Input.Visualization.Hvega     as KV
import qualified Knit.Report.Input.MarkDown.PandocMarkDown as KM
import qualified Knit.Report.Input.Html                    as KH

import qualified Graphics.Vega.VegaLite        as GV
import qualified Lucid                         as LH
import qualified Text.Blaze.Html               as BH
import qualified Text.Blaze.Html5               as BH
import qualified Text.Blaze.Html5.Attributes   as BHA
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Diagrams.Prelude              as D
import qualified Diagrams.Backend.SVG          as DSVG
--import           Diagrams.Backend.SVG
import qualified Graphics.Svg                  as SVG

import qualified Control.Kernmantle.Rope as Rope
import qualified Polysemy                      as P
import qualified Control.Arrow as A

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- logging
logLE :: K.LogSeverity -> T.Text -> KnitPipeline r () ()
logLE ls lt = proc _ -> do
  Rope.strand #log LogText -< (ls, lt)

-- add to document
addBlaze :: DocPipeline r BH.Html ()
addBlaze = proc html -> do
  Rope.strand #doc AddFrom -< (PE.ReadHtml, KH.htmlReaderOptions, LT.toStrict $ BH.renderHtml html)

addLucid :: DocPipeline r (LH.Html ()) ()
addLucid = proc html -> do
  Rope.strand #doc AddFrom -< (PE.ReadHtml, KH.htmlReaderOptions, LT.toStrict $ LH.renderText html)

addMarkDown :: DocPipeline r T.Text ()
addMarkDown = proc mdText -> do
  Rope.strand #doc AddFrom -< (PE.ReadMarkDown, KM.markDownReaderOptions, mdText)

addHvega :: P.Member KUI.UnusedId r
         => Maybe T.Text
         -> Maybe T.Text
         -> (a -> GV.VegaLite)
         -> DocPipeline r a T.Text
addHvega idTextM captionTextM makeVL =
  let getId = maybe (KUI.getNextUnusedId "figure") return
  in proc a -> do
    Rope.strand #doc Require -< K.VegaSupport
    idText <- (Rope.strand #knitCore $ A.Kleisli getId) -< idTextM
    addBlaze -< KV.placeVisualization idText captionTextM (makeVL a)
    A.returnA -< idText

addHvega' :: P.Member KUI.UnusedId r
         => Maybe T.Text
         -> Maybe T.Text
         -> GV.VegaLite
         -> DocPipeline r a T.Text
addHvega' idTextM captionTextM vl = addHvega idTextM captionTextM (const vl)

  
addDiagramAsSVGWithOptions :: P.Member KUI.UnusedId r
                           => Maybe T.Text -- ^ id attribute for figure.  Will use next unused "figure" id if Nothing
                           -> Maybe T.Text -- ^ caption for figure
                           -> DSVG.Options DSVG.SVG D.V2 Double
                           -> DocPipeline r (D.QDiagram DSVG.SVG D.V2 Double D.Any) T.Text
addDiagramAsSVGWithOptions idTextM captionTextM svgOptions =
  let getId = maybe (KUI.getNextUnusedId "figure") return
  in proc diagram -> do
    idText <- (Rope.strand #knitCore $ A.Kleisli getId) -< idTextM
    let blazeHtml =  BH.figure BH.! BHA.id (BH.toValue idText) $ do
                                                                   BH.preEscapedLazyText
                                                                     $ SVG.renderText
                                                                     $ D.renderDia DSVG.SVG svgOptions diagram
                                                                   maybe (return ()) (BH.figcaption . BH.toHtml) captionTextM
                                                               
    addBlaze -< blazeHtml
    A.returnA -< idText


addDiagramAsSVG  :: P.Member KUI.UnusedId r
                 => Maybe T.Text -- ^ id attribute for figure.  Will use next unused "figure" id if Nothing
                 -> Maybe T.Text -- ^ caption for figure
                 -> Double -- ^ width in pixels (?)
                 -> Double -- ^ height in pixels (?)                 -> Double
                 -> DocPipeline r (D.QDiagram DSVG.SVG D.V2 Double D.Any) T.Text
addDiagramAsSVG idTextM captionTextM wPixels hPixels =
  let getId = maybe (KUI.getNextUnusedId "figure") return
  in proc diagram -> do
     idText <- (Rope.strand #knitCore $ A.Kleisli getId) -< idTextM
     let svgOptions =
           DSVG.SVGOptions (D.dims2D wPixels hPixels) Nothing idText [] False
         blazeHtml =  BH.figure BH.! BHA.id (BH.toValue idText) $ do
                                                                   BH.preEscapedLazyText
                                                                     $ SVG.renderText
                                                                     $ D.renderDia DSVG.SVG svgOptions diagram
                                                                   maybe (return ()) (BH.figcaption . BH.toHtml) captionTextM
     
     addBlaze -< blazeHtml
     A.returnA -< idText
