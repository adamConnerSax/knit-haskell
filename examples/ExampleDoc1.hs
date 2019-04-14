{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import qualified Knit.Effects.Logger    as Log
import qualified Knit.Effects.PandocMonad as PM
import qualified Knit.Effects.Pandoc      as PE

import qualified Knit.Report.Pandoc            as PR
import qualified Polysemy as P

import           Control.Monad.IO.Class          (MonadIO(liftIO))
import qualified Data.Map                      as M
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                as T
import           Data.String.Here (here)
import qualified Graphics.Vega.VegaLite        as V

templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "knit-haskell example #1")
--  , ("tufte","True")
  ]

main :: IO ()
main = do
  let runEffects =
        PM.runPandocAndLoggingToIO Log.logAll
          . Log.wrapPrefix "Example1.Main"
          . fmap BH.renderHtml
      pandocToHtml =  PR.pandocWriterToBlazeDocument
        (Just "pandoc-templates/minWithVega-pandoc.html")
        templateVars
        PR.mindocOptionsF
  htmlAsTextE <- runEffects $ pandocToHtml $ makeDoc   
  case htmlAsTextE of
    Right htmlAsText ->
      T.writeFile "examples/html/example1.html"
        $ TL.toStrict
        $ htmlAsText
    Left err -> putStrLn $ "pandoc error: " ++ show err

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: (P.Member PE.ToPandoc effs
           , Log.LogWithPrefixesLE effs
           , PM.PandocEffects effs
           , MonadIO (P.Semantic effs)) => P.Semantic effs ()
makeDoc = Log.wrapPrefix "makeDoc" $ do
  Log.logLE Log.Info "adding some markdown..."
  PR.addMarkDown md1
  Log.logLE Log.Info "adding some latex..."
  PR.addMarkDown "## Some example latex"
  PR.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"
  Log.logLE Log.Info "adding a visualization..."
  PR.addMarkDown "## An example hvega visualization"
  PR.addHvega "someID" exampleVis

exampleVis =
  let cars =  V.dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []
      enc = V.encoding
        . V.position V.X [ V.PName "Horsepower", V.PmType V.Quantitative ]
        . V.position V.Y [ V.PName "Miles_per_Gallon", V.PmType V.Quantitative ]
        . V.color [ V.MName "Origin", V.MmType V.Nominal ]
      bkg = V.background "rgba(0, 0, 0, 0.05)"
  in V.toVegaLite [ bkg, cars, V.mark V.Circle [], enc [] ]  
