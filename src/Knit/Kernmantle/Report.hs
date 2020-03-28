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
  , module Knit.Report
  , module Knit.Kernmantle.Report
  , module Polysemy
  , module Control.Arrow
  ) where

-- for re-export
import Knit.Kernmantle
import Polysemy hiding (rewrite, transform) -- also something in Diagrams
import qualified Knit.Report as K
import Knit.Report
import Control.Arrow (returnA, (***), (&&&), (>>>), (<<<), (^>>), (>>^), first, second)

import qualified Knit.Effect.Pandoc       as PE
import qualified Knit.Effect.UnusedId                      as KUI
import qualified Knit.Report.Input.Visualization.Hvega     as KV
import qualified Knit.Report.Input.MarkDown.PandocMarkDown as KM
import qualified Knit.Report.Input.Html                    as KH

import qualified Graphics.Vega.VegaLite        as GV
import qualified Text.Pandoc                   as PA
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
logLEA :: K.LogSeverity -> KnitPipeline r T.Text ()
logLEA ls = proc lt -> do
  Rope.strand #log LogText -< (ls, lt)

-- This needs to be mantle polymorphic so we need to do it this way, with explicit type variables @r@, @m@, and @c.
-- Is there something simpler? 
wrapPrefixA :: (A.Arrow c, Rope.InRope "log" LogEff (Rope.Rope r m c))
            => T.Text
            -> Rope.Rope r m c a b
            -> Rope.Rope r m c a b
wrapPrefixA prefix pipeline = proc a -> do
  Rope.strand #log AddPrefix -< prefix
  b <- pipeline -< a
  Rope.strand #log RemovePrefix -< ()
  A.returnA -< b

-- add to document

-- HTML
addBlazeA :: DocPipeline r BH.Html ()
addBlazeA = proc html -> do
  Rope.strand #doc AddFrom -< (PE.ReadHtml, KH.htmlReaderOptions, LT.toStrict $ BH.renderHtml html)

addLucidA :: DocPipeline r (LH.Html ()) ()
addLucidA = proc html -> do
  Rope.strand #doc AddFrom -< (PE.ReadHtml, KH.htmlReaderOptions, LT.toStrict $ LH.renderText html)

-- Markdown
addMarkDownA :: DocPipeline r T.Text ()
addMarkDownA = proc mdText -> do
  Rope.strand #doc AddFrom -< (PE.ReadMarkDown, KM.markDownReaderOptions, mdText)

-- LaTeX
addLatexA :: DocPipeline r T.Text ()
addLatexA = proc t -> do
  Rope.strand #doc Require -< K.LatexSupport
  Rope.strand #doc AddFrom -< (K.ReadLaTeX, PA.def, t)

-- Vega-Lite (hvega)
addHvegaA :: P.Member KUI.UnusedId r
         => Maybe T.Text
         -> Maybe T.Text
         -> (a -> GV.VegaLite)
         -> DocPipeline r a T.Text
addHvegaA idTextM captionTextM makeVL =
  let getId = maybe (KUI.getNextUnusedId "figure") return
  in proc a -> do
    Rope.strand #doc Require -< K.VegaSupport
    idText <- (Rope.strand #knitCore $ A.Kleisli getId) -< idTextM
    addBlazeA -< KV.placeVisualization idText captionTextM (makeVL a)
    A.returnA -< idText

addHvegaA' :: P.Member KUI.UnusedId r
         => Maybe T.Text
         -> Maybe T.Text
         -> GV.VegaLite
         -> DocPipeline r () T.Text
addHvegaA' idTextM captionTextM vl = addHvegaA idTextM captionTextM (const vl)


-- Diagrams  
addDiagramAsSVGWithOptionsA :: P.Member KUI.UnusedId r
                            => Maybe T.Text -- ^ id attribute for figure.  Will use next unused "figure" id if Nothing
                            -> Maybe T.Text -- ^ caption for figure
                            -> DSVG.Options DSVG.SVG D.V2 Double
                            -> DocPipeline r (D.QDiagram DSVG.SVG D.V2 Double D.Any) T.Text
addDiagramAsSVGWithOptionsA idTextM captionTextM svgOptions =
  let getId = maybe (KUI.getNextUnusedId "figure") return
  in proc diagram -> do
    idText <- (Rope.strand #knitCore $ A.Kleisli getId) -< idTextM
    let blazeHtml =  BH.figure BH.! BHA.id (BH.toValue idText) $ do
                                                                   BH.preEscapedLazyText
                                                                     $ SVG.renderText
                                                                     $ D.renderDia DSVG.SVG svgOptions diagram
                                                                   maybe (return ()) (BH.figcaption . BH.toHtml) captionTextM
                                                               
    addBlazeA -< blazeHtml
    A.returnA -< idText


addDiagramAsSVGA  :: P.Member KUI.UnusedId r
                  => Maybe T.Text -- ^ id attribute for figure.  Will use next unused "figure" id if Nothing
                  -> Maybe T.Text -- ^ caption for figure
                  -> Double -- ^ width in pixels (?)
  -> Double -- ^ height in pixels (?)                 -> Double
                  -> DocPipeline r (D.QDiagram DSVG.SVG D.V2 Double D.Any) T.Text
addDiagramAsSVGA idTextM captionTextM wPixels hPixels =
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
     
     addBlazeA -< blazeHtml
     A.returnA -< idText
