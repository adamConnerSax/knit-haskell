{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Knit.Report                   as Knit
--import qualified Knit.Utilities.Streamly       as Knit.Streamly

import qualified Streamly.Prelude as Streamly

import qualified Control.Concurrent            as CC
import qualified Control.Monad.IO.Class        as MonadIO
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
  let template = Knit.FromIncludedTemplateDir "pandoc-adaptive-bootstrap-KH.html"
  pandocWriterConfig <- Knit.mkPandocWriterConfig template
                                               templateVars
                                               Knit.mindocOptionsF

  let knitConfig = (Knit.defaultKnitConfig Nothing)
        { Knit.outerLogPrefix = Just "CacheExample3.Main"
        , Knit.logIf = Knit.logAll
        , Knit.pandocWriterConfig = pandocWriterConfig
        }
  resE <- Knit.knitHtml knitConfig makeDoc

  case resE of
    Right htmlAsText ->
      Knit.writeAndMakePathLT "docs/cache_example.html" htmlAsText
    Left err -> putStrLn $ "Pandoc Error: " ++ show err

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: forall r.(Knit.KnitOne r, Knit.CacheEffectsD r) => Knit.Sem r ()
makeDoc = Knit.wrapPrefix "makeDoc" $ do

  Knit.logLE Knit.Info "Clearing \"cacheExample/test4.bin\" from cache..."
  Knit.clearIfPresent "cacheExample/test4.bin"
  Knit.logLE Knit.Info $ "retrieveOrMake list of data the first time.  Should make. But not deserialize upon use since the actual data is returned in the cache object.."
  testList1_C <- listLoaderWC
  Knit.logLE Knit.Info $ "retrieveOrMake list of data the second time.  Should load from disk. But not deserialize until use."
  testList2_C <- listLoaderWC
  Knit.logLE Knit.Info "Using first list via show"
  Knit.ignoreCacheTime testList1_C >>= MonadIO.liftIO . putStrLn . show
  Knit.logLE Knit.Info "Using second list via show"
  Knit.ignoreCacheTime testList2_C >>= MonadIO.liftIO . putStrLn . show

  return ()

listLoader :: (Knit.KnitEffects q, Knit.CacheEffectsD q) => Knit.Sem q [Int]
listLoader = Knit.ignoreCacheTimeM listLoaderWC

listLoaderWC :: (Knit.KnitEffects q, Knit.CacheEffectsD q)
               => Knit.Sem q (Knit.ActionWithCacheTime q [Int])
listLoaderWC = Knit.wrapPrefix "listLoaderWC" $ do
  Knit.logLE Knit.Diagnostic $ "listLoaderWC called"
  Knit.retrieveOrMake "cacheExample/test.bin" (pure ()) $ const $ do
    Knit.logLE Knit.Diagnostic "Waiting to make..."
    Knit.liftKnit $ CC.threadDelay 1000000
    Knit.logLE Knit.Diagnostic "Making test data"
    return  [1,10,100]


listLoader2 ::(Knit.KnitEffects q, Knit.CacheEffectsD q)
              => Knit.Sem q [Int]
listLoader2 =  Knit.ignoreCacheTimeM $ do
  cachedList <- listLoaderWC
  Knit.retrieveOrMake "cacheExample/test2.bin" cachedList $ \lInt -> do
    return $ fmap (*2) lInt

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
