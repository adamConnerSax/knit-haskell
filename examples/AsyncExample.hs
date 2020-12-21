{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}
module Main where

import qualified Knit.Report                   as K

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.String.Here               ( here )
import qualified Graphics.Vega.VegaLite        as V
import qualified Plots                         as P

import           Control.Concurrent            (threadDelay)

templateVars :: M.Map String String
templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "knit-haskell async example")
--  , ("tufte","True")
  ]

main :: IO ()
main = do
  let template = K.FromIncludedTemplateDir "pandoc-adaptive-bootstrap-KH.html"
  pandocWriterConfig <- K.mkPandocWriterConfig template
                                               templateVars
                                               K.mindocOptionsF
  let knitConfig = (K.defaultKnitConfig Nothing)
                   { K.outerLogPrefix = Just "AsyncExample.Main"
                   , K.logIf = K.logAll
                   , K.pandocWriterConfig = pandocWriterConfig
                   }
  knitConfig' <-  K.setKnitCapabilities (Just 4) knitConfig
  resE <- K.knitHtml knitConfig' makeDoc
  
  case resE of
    Right htmlAsText ->
      K.writeAndMakePathLT "docs/async_example.html" htmlAsText
    Left err -> putStrLn $ "Pandoc Error: " ++ show err

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: K.KnitOne effs => K.Sem effs ()
makeDoc = K.wrapPrefix "makeDoc" $ do
  K.logLE K.Info "adding some markdown..."
  K.addMarkDown md1
  K.logLE K.Info "adding some latex..."
  K.addMarkDown "## Some example latex"
  K.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"
  K.logLE K.Info "Launching some concurrent long-running computations..."
  asyncResultsM <- sequence <$> K.sequenceConcurrently [delay 2000 1, delay 2000 2, delay 2000 3]
  case asyncResultsM of
    Nothing -> K.logLE K.Error "One or more concurrent calculations failed."
    Just results -> do
      K.logLE K.Info "Concurrent calculations succeeded."
      K.addMarkDown $ "## Some concurrent calculation results: " <> (T.pack $ show results)
  K.logLE K.Info "Launching some concurrent long-running computations, this time using the WorkQueue"
  K.logLE K.Info "We've set the queue to allow 2 capabilities so 2 should launch and 1 wait and launch once another has finished."
  asyncQResultsM <- sequence <$> (K.queuedSequenceConcurrently $ fmap (K.mkAsyncable $ Just 1) [delay 2000 1, delay 4000 2, delay 1000 3])
  case asyncQResultsM of
    Nothing -> K.logLE K.Error "One or more concurrent calculations failed."
    Just results -> do
      K.logLE K.Info "Concurrent calculations succeeded."
      K.addMarkDown $ "## Some concurrent calculation results: " <> (T.pack $ show results)
{-  K.logLE K.Info "This time one will throw, should release capabilties anyway."
  K.logLE K.Info "We've set the queue to allow 2 capabilities so 2 should launch and 1 wait and launch once another has finished."
  asyncQResultsM <- sequence <$> (K.queuedSequenceConcurrently $ fmap (K.mkQueueableJob 1) [delayAndThrow 2000 1, delay 4000 2, delay 1000 3])
  case asyncQResultsM of
    Nothing -> K.logLE K.Error "One or more concurrent calculations failed."
    Just results -> do
      K.logLE K.Info "Concurrent calculations succeeded."
      K.addMarkDown $ "## Some concurrent calculation results: " <> (T.pack $ show results)
-}           
        
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
  return ()

-- long running computation
delay :: K.KnitEffects effs => Int -> Int -> K.Sem effs Int
delay msDelay val = K.wrapPrefix ("delay") $ do
  K.logLE K.Info $ "delaying " <> (T.pack $ show val) <> " for " <> (T.pack $ show msDelay) <> " ms"
  K.liftKnit (threadDelay $ 1000 * msDelay)
  K.logLE K.Info $ "done (" <> (T.pack $ show val) <> ")"  
  return val

-- long running computation
delayAndThrow :: K.KnitEffects effs => Int -> Int -> K.Sem effs Int
delayAndThrow msDelayBefore val = K.wrapPrefix ("delay") $ do
  K.logLE K.Info $ "delaying " <> (T.pack $ show val) <> " for " <> (T.pack $ show msDelayBefore) <> " ms"
  K.liftKnit (threadDelay $ 1000 * msDelayBefore)
  K.knitError $ "Exception in " <> (T.pack $ show val)  
  K.logLE K.Info $ "done (" <> (T.pack $ show val) <> ")"  
  return val

  

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

