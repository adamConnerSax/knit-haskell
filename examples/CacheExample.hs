{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Knit.Report                   as Knit
import qualified Knit.Utilities.Streamly       as Knit.Streamly
--import qualified Knit.Effect.Logger            as K

import qualified Streamly.Prelude as Streamly

import qualified Control.Concurrent            as CC
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
  let template = Knit.DefaultTemplate --K.FromIncludedTemplateDir "mindoc-pandoc-KH.html"
  pandocWriterConfig <- Knit.mkPandocWriterConfig template
                                               templateVars
                                               Knit.mindocOptionsF

  let knitConfig = Knit.defaultKnitConfig
        { Knit.outerLogPrefix = Just "CacheExample.Main"
        , Knit.logIf = Knit.logAll
        , Knit.pandocWriterConfig = pandocWriterConfig
        }                                               
  resE <- Knit.knitHtml knitConfig makeDoc

  case resE of
    Right htmlAsText ->
      Knit.writeAndMakePathLT "examples/html/cache_example.html" htmlAsText
    Left err -> putStrLn $ "Pandoc Error: " ++ show err

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: Knit.KnitOne r => Knit.Sem r ()
makeDoc = Knit.wrapPrefix "makeDoc" $ do
  Knit.logLE Knit.Info "adding some markdown..."
  Knit.addMarkDown md1
  Knit.logLE Knit.Info "adding some latex..."
  Knit.addMarkDown "## Some example latex"
  Knit.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"
  Knit.logLE Knit.Info "Clearing cacheExample from cache..."
  Knit.clearIfPresent "cacheExample/test.sbin"
  Knit.logLE Knit.Info "asynchronously retrieving or making stream of data, then retrieving on this thread to test atomic cache."
  Knit.logLE Knit.Info $ "This should force the one on this thread to block until the async one,"
    <> " which will try the cache first"
    <> ", tries memory, then disk, then makes the data."
  Knit.logLE Knit.Info "At which point this one will unblock and see the data in the in-memory-cache."
  testListA <- Knit.async $ Knit.wrapPrefix "ASYNC" $ do
    dat <- streamLoader
    Knit.logLE Knit.Diagnostic "Waiting to return from Async"
    Knit.liftKnit $ CC.threadDelay 1000000
    return dat
  Knit.liftKnit $ CC.threadDelay 100000    
  testList <- streamLoader
  testListM <- Knit.await testListA
  case testListM of
    Nothing -> Knit.logLE Knit.Diagnostic "Error in async retrieve"
    Just l -> do
      Knit.logLE Knit.Diagnostic "List returned from async.  Logging async then sync."
      Knit.logLE Knit.Diagnostic $ "async:" <> (T.pack $ show l)
      Knit.logLE Knit.Diagnostic $ "sync:" <> (T.pack $ show testList)
  Knit.logLE Knit.Info "Demonstrating cache dependencies"
  Knit.logLE Knit.Info "streamLoader2 depends on streamLoader and should rebuild if streamLoader has been rebuilt since streamLoader2 was cached."
  Knit.logLE Knit.Info "Calling streamLoader2 the first time"
  sl2a <- streamLoader2 -- builds the first time
  Knit.logLE Knit.Info "Calling streamLoader2 again. Should load from cache."
  sl2b <- streamLoader2 -- loads from cache
  Knit.logLE Knit.Info "Removing cached streamLoader1 result, then calling streamLoader 2 again. Should rebuild."
  Knit.clear "cacheExample/test.sbin" -- remove sl1
  sl2c <- streamLoader2 -- should rebuild
  Knit.logLE Knit.Info $ "streamLoader2=" <> (T.pack $ show sl2c)
  Knit.logLE Knit.Info "adding a visualization..."
  Knit.addMarkDown "## An example hvega visualization"
  _ <- Knit.addHvega Nothing (Just "From the cars data-set") exampleVis
  Knit.addMarkDown
    "## Example Diagrams visualizations, the second using the plots library."
  Knit.logLE Knit.Info "adding a Diagrams example and plots example..."
  _ <- Knit.addDiagramAsSVG Nothing (Just "Example diagram") 300 300 exampleDiagram
  _ <- Knit.addDiagramAsSVG
    Nothing
    (Just "Example diagrams visualization using the Plots library")
    300
    300
    samplePlot
  Knit.logLE Knit.Info "Retrieving that stuff from the cache or running if required."
  Knit.addMarkDown $ "## Caching: List=" <> (T.pack $ show testList)
  return ()

streamLoader :: Knit.KnitEffects q => Knit.Sem q [Int]
streamLoader = Streamly.toList $ Knit.ignoreCacheTimeStream $ streamLoaderWC

streamLoaderWC :: Knit.KnitEffects q => Knit.Sem q (Knit.StreamWithCacheTime q Int)
streamLoaderWC = Knit.wrapPrefix "streamLoaderWC" $ do
  Knit.logLE Knit.Diagnostic $ "streamLoaderWC called"
  Knit.retrieveOrMakeStream "cacheExample/test.sbin" Nothing $ do               
    Streamly.yieldM $ Knit.logLE Knit.Diagnostic "Waiting to make..."
    Streamly.yieldM $ Knit.liftKnit $ CC.threadDelay 1000000                           
    Streamly.yieldM $ Knit.logLE Knit.Diagnostic "Making test data"
    Knit.Streamly.streamlyToKnitS
      $ Streamly.fromList  [1,10,100]
               

streamLoader2 ::  Knit.KnitEffects q => Knit.Sem q [Int]
streamLoader2 = Streamly.toList $ Knit.ignoreCacheTimeStream $ do
  (Knit.WithCacheTime cTime sInt) <- streamLoaderWC
  Knit.retrieveOrMakeStream "cacheExample/test2.sbin" (Just cTime) $ do
    Streamly.map (*2) sInt
               
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
    Knit.# Knit.reflectY
    <>  Knit.vrule 1
    <>  hilbert (n - 1)
    <>  Knit.hrule 1
    <>  hilbert (n - 1)
    <>  Knit.vrule (-1)
    <>  hilbert' (n - 1)
    Knit.# Knit.reflectX
  where hilbert' m = hilbert m Knit.# Knit.rotateBy (1 / 4)

exampleDiagram :: Knit.Diagram Knit.SVG
exampleDiagram =
  Knit.frame 1 . Knit.lw Knit.medium . Knit.lc Knit.darkred . Knit.strokeT $ hilbert 5


-- example using Plots (as an example of using Diagrams)
samplePlot :: Knit.Diagram Knit.SVG
samplePlot = P.renderAxis logAxis

logData = [Knit.V2 1 10, Knit.V2 2 100, Knit.V2 2.5 316, Knit.V2 3 1000]

logAxis :: P.Axis Knit.SVG Knit.V2 Double
logAxis = P.r2Axis Knit.&~ do
  P.scatterPlot' logData

  P.yAxis P.&= do
    P.logScale Knit..= P.LogAxis
    P.majorTicksFunction Knit..= P.logMajorTicks 5 -- <> pure [1]

