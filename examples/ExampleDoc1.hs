{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Monad.Freer.Logger    as Log
import qualified Control.Monad.Freer.PandocMonad
                                               as FR
import qualified Text.Pandoc.Report            as P
import qualified Control.Monad.Freer.Pandoc      as P

import           Control.Monad.IO.Class          (MonadIO(liftIO))
import qualified Data.Map                      as M
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
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
        FR.runPandocAndLoggingToIO Log.logAll
          . Log.wrapPrefix "Example1.Main"
          . fmap BH.renderHtml
      pandocToHtml =  P.pandocWriterToBlazeDocument
        (Just "pandoc-templates/minWithVega-pandoc.html")
        templateVars
        P.mindocOptionsF
  htmlAsTextE <- runEffects $ pandocToHtml $ makeDoc   
  case htmlAsTextE of
    Right htmlAsText ->
      T.writeFile "examples/html/example1.html"
        $ TL.toStrict
        $ htmlAsText
    Left err -> putStrLn $ "pandoc error: " ++ show err

md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: (P.Member P.ToPandoc effs
           , Log.LogWithPrefixes effs
           , FR.PandocEffects effs
           , MonadIO (P.Eff effs)) => P.Eff effs ()
makeDoc = Log.wrapPrefix "makeDoc" $ do
  Log.logLE Log.Info "adding some markdown..."
  P.addMarkDown md1
  Log.logLE Log.Info "adding some latex..."
  P.addMarkDown "## Some example latex"
  P.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"
  Log.logLE Log.Info "adding a visualization..."
  P.addMarkDown "## An example hvega visualization"
  P.addHvega "someID" exampleVis

exampleVis =
  let cars =  V.dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []
      enc = V.encoding
        . V.position V.X [ V.PName "Horsepower", V.PmType V.Quantitative ]
        . V.position V.Y [ V.PName "Miles_per_Gallon", V.PmType V.Quantitative ]
        . V.color [ V.MName "Origin", V.MmType V.Nominal ]
      bkg = V.background "rgba(0, 0, 0, 0.05)"
  in V.toVegaLite [ bkg, cars, V.mark V.Circle [], enc [] ]  
