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
import qualified Data.Text                     as T
import           Data.String.Here (here)
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
  let writeNamedHtml (K.DocWithInfo (K.PandocInfo n _) lt)
        = T.writeFile (T.unpack $ "examples/html/" <> n <> ".html") $ TL.toStrict lt
      writeAllHtml = fmap (const ()) . traverse writeNamedHtml
      template = K.FromIncludedTemplateDir "pandoc-adaptive-bootstrap-KH.html"
  tvWithCss <- K.addCss (K.FromIncludedCssDir "pandoc-bootstrap.css") templateVars
  pandocWriterConfig <- K.mkPandocWriterConfig template  tvWithCss K.mindocOptionsF
  resE <- K.knitHtmls (Just "SimpleExample.Main") K.logAll pandocWriterConfig $ do
    K.newPandoc (K.PandocInfo "multi_doc1" M.empty) makeDoc1
    K.newPandoc (K.PandocInfo "multi_doc2" M.empty) makeDoc2
  case resE of
    Right namedDocs -> writeAllHtml namedDocs 
    Left err -> putStrLn $ "pandoc error: " ++ show err
    
md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc1 :: K.KnitOne effs => K.Sem effs ()
makeDoc1 = K.wrapPrefix "makeDoc1" $ do
  K.logLE K.Info "adding some markdown."
  K.addMarkDown md1
  K.logLE K.Info "adding some latex."
  K.addMarkDown "## Some example latex (Doc 1)"
  K.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"
  K.logLE K.Info "adding a visualization."
  K.addMarkDown "## An example hvega visualization (Doc 1)"
  _ <- K.addHvega Nothing Nothing exampleVis
  return ()

md2 :: T.Text
md2 = [here|
## Some example markdown
* This is some more markdown! Now for document 2. It's still a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc2 :: K.KnitOne effs => K.Sem effs ()
makeDoc2 = K.wrapPrefix "makeDoc2" $ do
  K.logLE K.Info "adding some markdown."
  K.addMarkDown md2
  K.logLE K.Info "adding some latex."
  K.addMarkDown "## Some example latex (Doc 2)"
  K.addLatex "A different equation: $a^2 + b^2 = c^2$"
  K.logLE K.Info "adding a visualization."
  K.addMarkDown "## An example hvega visualization (Doc 2)"
  _ <- K.addHvega Nothing Nothing exampleVis
  return ()

exampleVis :: V.VegaLite
exampleVis =
  let cars =  V.dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []
      enc = V.encoding
        . V.position V.X [ V.PName "Horsepower", V.PmType V.Quantitative ]
        . V.position V.Y [ V.PName "Miles_per_Gallon", V.PmType V.Quantitative ]
        . V.color [ V.MName "Origin", V.MmType V.Nominal ]
      bkg = V.background "rgba(0, 0, 0, 0.05)"
  in V.toVegaLite [ bkg, cars, V.mark V.Circle [], enc [] ]  

