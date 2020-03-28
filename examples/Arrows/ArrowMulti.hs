{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}
module Main where

import qualified Knit.Kernmantle.Report                   as K

import qualified Data.Map                      as M
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                     as T
import           Data.String.Here               ( here )
import qualified Graphics.Vega.VegaLite        as V

templateVars :: M.Map String String
templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "knit-haskell simple multi-doc example")
--  , ("tufte","True")
  ]

main :: IO ()
main = do
  let template = K.FromIncludedTemplateDir "pandoc-adaptive-bootstrap-KH.html"
  tvWithCss <- K.addCss (K.FromIncludedCssDir "pandoc-bootstrap.css")
                        templateVars
  pandocWriterConfig <- K.mkPandocWriterConfig template
                                               tvWithCss
                                               K.mindocOptionsF
  let knitConfig = K.defaultKnitConfig
        { K.outerLogPrefix = Just "ArrowMulti.Main"
        , K.logIf = K.logAll
        , K.pandocWriterConfig = pandocWriterConfig
        }

  resE <- K.runDocsPipeline knitConfig docsPipeline ()
  case resE of
    Right namedDocs ->
      K.writeAllPandocResultsWithInfoAsHtml "examples/html" namedDocs
    Left err -> putStrLn $ "pandoc error: " ++ show err

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

docsPipeline :: forall r. (K.KnitMany r, K.KnitOne r) => K.DocsPipeline r () ()
docsPipeline = proc _ -> do
  K.newPandocA (K.PandocInfo "arrowM ulti_doc1" M.empty) docPipeline1 -< () 
  K.newPandocA (K.PandocInfo "arrowMulti_doc2" M.empty) docPipeline2 -< ()
  
docPipeline1 :: K.KnitOne r => K.DocPipeline r () ()
docPipeline1 = K.wrapPrefixA "makeDoc1" $ proc _  -> do
  K.logLEA K.Info -< "adding some markdown."
  K.addMarkDownA -< md1
  K.logLEA K.Info -< "adding some latex."
  K.addMarkDownA -< "## Some example latex (Doc 1)"
  K.addLatexA -< "Overused favorite equation: $e^{i\\pi} + 1 = 0$"
  K.logLEA K.Info -< "adding a visualization."
  K.addMarkDownA -< "## An example hvega visualization (Doc 1)"
  _ <- K.addHvegaA Nothing Nothing id -< exampleVis
  K.returnA -< ()

md2 :: T.Text
md2 = [here|
## Some example markdown
* This is some more markdown! Now for document 2. It's still a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

docPipeline2 :: K.KnitOne r => K.DocPipeline r () ()
docPipeline2 = K.wrapPrefixA "makeDoc2" $ proc _ -> do
  K.logLEA K.Info -< "adding some markdown."
  K.addMarkDownA -< md2
  K.logLEA K.Info -< "adding some latex."
  K.addMarkDownA -< "## Some example latex (Doc 2)"
  K.addLatexA -< "A different equation: $a^2 + b^2 = c^2$"
  K.logLEA K.Info -< "adding a visualization."
  K.addMarkDownA -< "## An example hvega visualization (Doc 2)"
  _ <- K.addHvegaA Nothing Nothing id -< exampleVis
  K.returnA -< ()

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

