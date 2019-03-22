{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Html.Lucid.Report
  (
    makeReportHtml
  , placeVisualization
  , placeTextSection
  , latexToHtml
  , latex_
  )
where

--import           Control.Monad.Morph        (generalize, hoist, lift)
--import           Control.Monad.Trans        (lift)
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as LT
import qualified Graphics.Vega.VegaLite     as GV
import qualified Lucid                      as H
import qualified Text.Pandoc                as P

import qualified Control.Monad.Freer.Html   as FH
--import qualified Control.Monad.Freer  as FR

latexToHtml :: T.Text -> H.Html ()
latexToHtml lText = do
  let latexReadOptions = P.def
      htmlWriteOptions = P.def { P.writerHTMLMathMethod = P.MathJax "" }
      asHtml = P.readLaTeX latexReadOptions lText >>= P.writeHtml5String htmlWriteOptions
  case P.runPure asHtml of
    Left err       -> H.span_ (H.toHtml $ show err)
    Right htmlText -> H.span_ (H.toHtmlRaw htmlText)

latex_ :: T.Text -> H.Html ()
latex_ = latexToHtml


mathJaxScript :: H.Html ()
mathJaxScript = H.script_ [H.src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML", H.async_ ""] ""

vegaScripts2 :: H.Html ()
vegaScripts2 = do
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega@4.4.0"] ""
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega-lite@3.0.0-rc11"] ""
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega-embed@3.28.0"] ""

vegaScripts3 :: H.Html ()
vegaScripts3 = do
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega@4.4.0/build/vega.js"] ""
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega-lite@3.0.0-rc12/build/vega-lite.js"] ""
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega-embed@3.29.1/build/vega-embed.js"] ""

tufteSetup :: H.Html ()
tufteSetup = do
   H.link_ [H.rel_ "stylesheet", H.href_ "https://cdnjs.cloudflare.com/ajax/libs/tufte-css/1.4/tufte.min.css"]
   H.meta_ [H.name_ "viewport", H.content_"width=device-width, initial-scale=1"]

makeReportHtml :: T.Text -> H.Html a -> H.Html a
makeReportHtml title reportHtml = H.html_ $ head >> H.body_ (H.article_ reportHtml) where
  head :: H.Html () = H.head_ (do
                                  H.title_ (H.toHtmlRaw title)
                                  tufteSetup
                                  mathJaxScript
                                  vegaScripts2
                                  return ()
                              )

placeVisualization :: T.Text -> GV.VegaLite -> H.Html ()
placeVisualization idText vl =
  let vegaScript :: T.Text = T.decodeUtf8 $ BS.toStrict $ A.encodePretty $ GV.fromVL vl
      script = "var vlSpec=\n" <> vegaScript <> ";\n" <> "vegaEmbed(\'#" <> idText <> "\',vlSpec);"
  in H.figure_ [H.id_ idText] (H.script_ [H.type_ "text/javascript"]  (H.toHtmlRaw script))

placeTextSection :: H.Html () -> H.Html ()
placeTextSection x = H.section_ [{- attributes/styles here -}] x


-- utilities for lifting through

--htmlToIOLogged :: H.Html a -> SL.Logger (H.HtmlT IO) a
--htmlToIOLogged = SL.liftPureAction
