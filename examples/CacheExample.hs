{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Knit.Report                   as K
import qualified Knit.Effect.Logger            as K
import qualified Knit.Report.Cache             as KC

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.String.Here               ( here )
import qualified Graphics.Vega.VegaLite        as V
import qualified Plots                         as P


templateVars :: M.Map String String
templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "knit-haskell cache example")
--  , ("tufte","True")
  ]

main :: IO ()
main = do
  let template = K.DefaultTemplate --K.FromIncludedTemplateDir "mindoc-pandoc-KH.html"
  pandocWriterConfig <- K.mkPandocWriterConfig template
                                               templateVars
                                               K.mindocOptionsF

  let knitConfig = K.defaultKnitConfig
        { K.outerLogPrefix = Just "AsyncExample.Main"
        , K.logIf = K.logAll
        , K.pandocWriterConfig = pandocWriterConfig
        }                                               
  resE <- K.knitHtml knitConfig makeDoc

  case resE of
    Right htmlAsText ->
      K.writeAndMakePathLT "examples/html/cache_example.html" htmlAsText
    Left err -> putStrLn $ "Pandoc Error: " ++ show err

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: K.KnitOne r => K.Sem r ()
makeDoc = K.wrapPrefix "makeDoc" $ do
  K.logLE K.Info "adding some markdown..."
  K.addMarkDown md1
  K.logLE K.Info "adding some latex..."
  K.addMarkDown "## Some example latex"
  K.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"
  K.logLE K.Info "Creating cached action..."
  let doubleListCA :: KC.Cached '[K.Logger K.LogEntry] [Double] = KC.cacheAction id "cacheExample/doubleList" (doSomethingEffectful 5 2.2)
  K.logLE K.Info "adding a visualization..."
  K.addMarkDown "## An example hvega visualization"
  _ <- K.addHvega Nothing (Just "From the cars data-set") exampleVis
  K.addMarkDown
    "## Example Diagrams visualizations, the second using the plots library."
  K.logLE K.Info "adding a Diagrams example and plots example..."
  _ <- K.addDiagramAsSVG Nothing (Just "Example diagram") 300 300 exampleDiagram
  _ <- K.addDiagramAsSVG
    Nothing
    (Just "Example diagrams visualization using the Plots library")
    300
    300
    samplePlot
  K.logLE K.Info "Retrieving that stuff from the cache or running if required."
  makeDoubleListDocPart doubleListCA
  heldDoubleList  <- KC.useCached doubleListCA
  -- NB: the below only requires type-annotation because it's not used anywhere else (except via "show") so it's
  -- type can't be inferred.  Normally that wouldn't be the case.
  cachedDoubleList :: [Double] <- KC.useCached $ KC.cacheRetrieval id "cacheExample/doubleList" 
  K.addMarkDown $ "## Caching: heldDoubleList=" <> (T.pack $ show heldDoubleList) <> "; retrieved=" <> (T.pack $ show cachedDoubleList)
  return ()

doSomethingEffectful :: K.Member (K.Logger K.LogEntry) r => Int -> Double -> K.Sem r [Double]
doSomethingEffectful n x = do
  K.logLE K.Info "Doing the effectful thing!"
  return $ replicate n x

makeDoubleListDocPart :: (K.KnitOne r, K.Members es r) => KC.Cached es [Double] -> K.Sem r ()
makeDoubleListDocPart ca = do
  ds <- KC.useCached ca
  K.logLE K.Info $ "double list from useCached is" <> (T.pack $ show ds)

-- example using HVega  
exampleVis :: V.VegaLite
exampleVis =
  let cars =
          V.dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []
      enc =
          V.encoding
            . V.position V.X [V.PName "Horsepower", V.PmType V.Quantitative]
            . V.position V.Y [V.PName "Miles_per_Gallon", V.PmType V.Quantitative]
            . V.color [V.MName "Origin", V.MmType V.Nominal]
      bkg = V.background "rgba(0, 0, 0, 0.05)"
  in  V.toVegaLite [bkg, cars, V.mark V.Circle [], enc []]


-- sampleDiagram
-- from <https://archives.haskell.org/projects.haskell.org/diagrams/gallery/Hilbert.html>

hilbert 0 = mempty
hilbert n =
  hilbert' (n - 1)
    K.# K.reflectY
    <>  K.vrule 1
    <>  hilbert (n - 1)
    <>  K.hrule 1
    <>  hilbert (n - 1)
    <>  K.vrule (-1)
    <>  hilbert' (n - 1)
    K.# K.reflectX
  where hilbert' m = hilbert m K.# K.rotateBy (1 / 4)

exampleDiagram :: K.Diagram K.SVG
exampleDiagram =
  K.frame 1 . K.lw K.medium . K.lc K.darkred . K.strokeT $ hilbert 5


-- example using Plots (as an example of using Diagrams)
samplePlot :: K.Diagram K.SVG
samplePlot = P.renderAxis logAxis

logData = [K.V2 1 10, K.V2 2 100, K.V2 2.5 316, K.V2 3 1000]

logAxis :: P.Axis K.SVG K.V2 Double
logAxis = P.r2Axis K.&~ do
  P.scatterPlot' logData

  P.yAxis P.&= do
    P.logScale K..= P.LogAxis
    P.majorTicksFunction K..= P.logMajorTicks 5 -- <> pure [1]

