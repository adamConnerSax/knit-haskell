{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}
module Main where

import qualified Knit.Report as K    

import qualified Data.Map                      as M
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                as T
import           Data.String.Here (here)
import qualified Graphics.Vega.VegaLite        as V

templateVars :: M.Map String String
templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "knit-haskell simple example")
--  , ("tufte","True")
  ]

main :: IO ()
main = do
  let pandocWriterConfig = K.PandocWriterConfig (Just "pandoc-templates/minWithVega-pandoc.html")  templateVars K.mindocOptionsF
  resE <- K.knitHtml (Just "SimpleExample.Main") K.logAll pandocWriterConfig makeDoc
  case resE of
    Right htmlAsText ->
      T.writeFile "examples/html/example_simple.html"
        $ TL.toStrict
        $ htmlAsText
    Left err -> putStrLn $ "Pandoc Error: " ++ show err

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: (K.Member K.ToPandoc effs -- required for the single-document variant
           , K.PandocEffects effs -- all effects for knitting
           ) 
        => K.Sem effs ()
makeDoc = K.wrapPrefix "makeDoc" $ do
  K.logLE K.Info "adding some markdown..."
  K.addMarkDown md1
  K.logLE K.Info "adding some latex..."
  K.addMarkDown "## Some example latex"
  K.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"
  K.logLE K.Info "adding a visualization..."
  K.addMarkDown "## An example hvega visualization"
  K.addHvega "someID" exampleVis

exampleVis :: V.VegaLite
exampleVis =
  let cars =  V.dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []
      enc = V.encoding
        . V.position V.X [ V.PName "Horsepower", V.PmType V.Quantitative ]
        . V.position V.Y [ V.PName "Miles_per_Gallon", V.PmType V.Quantitative ]
        . V.color [ V.MName "Origin", V.MmType V.Nominal ]
      bkg = V.background "rgba(0, 0, 0, 0.05)"
  in V.toVegaLite [ bkg, cars, V.mark V.Circle [], enc [] ]  
