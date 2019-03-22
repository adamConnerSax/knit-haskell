{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Html.Blaze.Report
  ( makeReportHtml
  , placeVisualization
  , placeTextSection
  , latexToHtml
--  , htmlToIOLogged
  , latex_
  )
where

import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Graphics.Vega.VegaLite        as GV
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as HA
import qualified Text.Pandoc                   as P

import qualified Control.Monad.Freer.Html      as FH
--import qualified Control.Monad.Freer  as FR

latexToHtml :: T.Text -> H.Html
latexToHtml lText = do
  let
    latexReadOptions = P.def
    htmlWriteOptions = P.def { P.writerHTMLMathMethod = P.MathJax "" }
    asHtml =
      P.readLaTeX latexReadOptions lText >>= P.writeHtml5String htmlWriteOptions
  case P.runPure asHtml of
    Left  err      -> H.span (H.toHtml $ show err)
    Right htmlText -> H.span (H.preEscapedToHtml htmlText)

latex_ :: T.Text -> H.Html
latex_ = latexToHtml

mathJaxScript :: H.Html
mathJaxScript =
  H.script
    ! HA.src
        "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML"
    ! HA.async ""
    $ ""

vegaScripts2 :: H.Html
vegaScripts2 = do
  H.script ! HA.src "https://cdn.jsdelivr.net/npm/vega@4.4.0" $ ""
  H.script ! HA.src "https://cdn.jsdelivr.net/npm/vega-lite@3.0.0-rc11" $ ""
  H.script ! HA.src "https://cdn.jsdelivr.net/npm/vega-embed@3.28.0" $ ""

vegaScripts3 :: H.Html
vegaScripts3 = do
  H.script ! HA.src "https://cdn.jsdelivr.net/npm/vega@4.4.0/build/vega.js" $ ""
  H.script
    ! HA.src
        "https://cdn.jsdelivr.net/npm/vega-lite@3.0.0-rc12/build/vega-lite.js"
    $ ""
  H.script
    ! HA.src
        "https://cdn.jsdelivr.net/npm/vega-embed@3.29.1/build/vega-embed.js"
    $ ""

tufteSetup :: H.Html
tufteSetup = do
  H.link ! HA.rel "stylesheet" ! HA.href
    "https://cdnjs.cloudflare.com/ajax/libs/tufte-css/1.4/tufte.min.css"
  H.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1"

makeReportHtml :: T.Text -> H.Html -> H.Html
makeReportHtml title reportHtml = H.html $ do
  H.docTypeHtml $ do
    H.head $ do
      H.title (H.toHtml title)
      tufteSetup
      mathJaxScript
      vegaScripts2
    H.body $ H.article $ reportHtml

placeVisualization :: T.Text -> GV.VegaLite -> H.Html
placeVisualization idText vl =
  let vegaScript :: T.Text =
        T.decodeUtf8 $ BS.toStrict $ A.encodePretty $ GV.fromVL vl
      script =
        "var vlSpec=\n"
          <> vegaScript
          <> ";\n"
          <> "vegaEmbed(\'#"
          <> idText
          <> "\',vlSpec);"
  in  H.figure ! HA.id (H.toValue idText) $ do
        H.script ! HA.type_ "text/javascript" $ H.preEscapedToHtml script

placeTextSection :: H.Html -> H.Html
placeTextSection x = H.section x

