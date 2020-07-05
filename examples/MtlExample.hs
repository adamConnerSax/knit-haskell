{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified Knit.Report                   as K

import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Map                      as M
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                     as T
import           Data.String.Here               ( here )
import qualified Graphics.Vega.VegaLite        as V

import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , runReaderT
                                                )

templateVars :: M.Map String String
templateVars = M.fromList
  [ ("lang"     , "English")
  , ("author"   , "Adam Conner-Sax")
  , ("pagetitle", "knit-haskell mtl example")
--  , ("tufte","True")
  ]

-- A demo application stack 
newtype MyApp env a = MyStack { unMyApp :: ReaderT env IO a } deriving (Functor, Applicative, Monad, MonadIO)

type ExampleApp = MyApp T.Text

runExampleApp :: T.Text -> ExampleApp a -> IO a
runExampleApp t = flip runReaderT t . unMyApp

getEnv :: MyApp env env
getEnv = MyStack $ ask

main :: IO ()
main = do
  let template = K.FromIncludedTemplateDir "pandoc-bootstrap-KH.html"
  templateVarsWithCss <- K.addCss
    (K.FullySpecifiedCssPath "css/pandoc-bootstrap.css")
    templateVars
  pandocWriterConfig <- K.mkPandocWriterConfig template
                                               templateVarsWithCss
                                               K.mindocOptionsF

  let knitConfig = (K.defaultKnitConfig Nothing)
        { K.outerLogPrefix = Just "MtlExample.Main"
        , K.logIf = K.logAll
        , K.pandocWriterConfig = pandocWriterConfig
        }
  resE <- runExampleApp "This is from the MyApp environment."
    $ K.knitHtml knitConfig makeDoc
  case resE of
    Right htmlAsText ->
      K.writeAndMakePathLT "docs/mtl_example.html" htmlAsText
    Left err -> putStrLn $ "Pandoc error: " ++ show err

md1 :: T.Text
md1 = [here|
## Some example markdown
* [Markdown][MarkdownLink] is a nice way to write formatted notes with a minimum of code.
* It supports links and tables and some *styling* information.

[MarkDownLink]:<https://pandoc.org/MANUAL.html#pandocs-markdown>
|]

makeDoc :: (K.KnitOne effs, K.KnitBase ExampleApp effs) => K.Sem effs ()
makeDoc = K.wrapPrefix "makeDoc" $ do
  K.logLE K.Info "adding some markdown..."
  K.addMarkDown md1

  K.logLE K.Info "adding some latex..."
  K.addMarkDown "## Some example latex"
  K.addLatex "Overused favorite equation: $e^{i\\pi} + 1 = 0$"

  K.logLE K.Info "adding a visualization..."
  K.addMarkDown "## An example hvega visualization"
  _ <- K.addHvega Nothing Nothing exampleVis

  K.logLE K.Info
          "Retrieving some text from the base monad and current date-time."
  envText <- K.liftKnit @ExampleApp getEnv
  curTime <- K.getCurrentTime
  K.addMarkDown
    "## An example of getting env from a base monad, and time from the Pandoc Effects."
  K.addMarkDown $ envText <> "\n\n" <> (T.pack $ show curTime)

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
