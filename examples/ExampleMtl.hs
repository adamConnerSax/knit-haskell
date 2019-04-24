{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified Knit.Report as K    

import           Control.Monad.IO.Class          (MonadIO)
import qualified Data.Map                      as M
--import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                as T
import           Data.String.Here (here)
import qualified Graphics.Vega.VegaLite        as V

import           Control.Monad.Reader (ReaderT, ask, runReaderT)

templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "knit-haskell example #1")
--  , ("tufte","True")
  ]

main :: IO ()
main = do
  let pandocWriterConfig = K.PandocWriterConfig (Just "pandoc-templates/minWithVega-pandoc.html")  templateVars K.mindocOptionsF
  resE <- K.runIO $ flip runReaderT "This is from the environment." $ unMyStack $ (K.knitHtml @ExampleApp) (Just "ExampleMtl.Main") K.logAll pandocWriterConfig makeDoc
  case resE of
    Right htmlAsText ->
      T.writeFile "examples/html/mtl_example.html"
        $ TL.toStrict
        $ htmlAsText
    Left _ -> return ()

newtype MyApp env a = MyStack { unMyStack :: ReaderT env K.PandocIO a } deriving (Functor, Applicative, Monad, K.MonadError K.PandocError, K.PandocMonad, MonadIO)

getEnv :: MyApp env env
getEnv = MyStack $ ask

type ExampleApp = MyApp T.Text

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: (K.Member K.ToPandoc effs
           , K.LogWithPrefixesLE effs
           , K.PandocEffects effs
           , MonadIO (K.Semantic effs)
           , K.KnitBase ExampleApp effs) => K.Semantic effs ()
makeDoc = K.wrapPrefix "makeDoc" $ do
  K.logLE K.Info "adding some markdown..."
  K.addMarkDown md1

  K.logLE K.Info "adding some latex..."
  K.addMarkDown "## Some example latex"
  K.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"

  K.logLE K.Info "adding a visualization..."
  K.addMarkDown "## An example hvega visualization"
  K.addHvega "someID" exampleVis

  K.logLE K.Info "Retrieving some text from the base monad..."
  envText <- K.liftKnit @ExampleApp getEnv
  K.addMarkDown "## An example of getting env from a base monad."
  K.addMarkDown envText

exampleVis =
  let cars =  V.dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []
      enc = V.encoding
        . V.position V.X [ V.PName "Horsepower", V.PmType V.Quantitative ]
        . V.position V.Y [ V.PName "Miles_per_Gallon", V.PmType V.Quantitative ]
        . V.color [ V.MName "Origin", V.MmType V.Nominal ]
      bkg = V.background "rgba(0, 0, 0, 0.05)"
  in V.toVegaLite [ bkg, cars, V.mark V.Circle [], enc [] ]  
