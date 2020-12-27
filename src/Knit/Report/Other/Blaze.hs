{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Knit.Report.Other.Blaze
Description : Support functions for simple reports in Blaze
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to support some simple reports using only Blaze.

Using the Pandoc framework instead is recommended.  
-}
module Knit.Report.Other.Blaze
  (
    -- * Add relevant headers, scripts
    makeReportHtml
    -- * add report pieces
  , placeVisualization
  , placeTextSection
  , latexToHtml
  , latex_
  )
where

import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.ByteString.Lazy.Char8    as BS
import qualified Graphics.Vega.VegaLite        as GV
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as HA
import qualified Text.Pandoc                   as P

-- | Convert Latex to Blaze Html
latexToHtml :: Text -> H.Html
latexToHtml lText = do
  let
    latexReadOptions = P.def
    htmlWriteOptions = P.def { P.writerHTMLMathMethod = P.MathJax "" }
    asHtml =
      P.readLaTeX latexReadOptions lText >>= P.writeHtml5String htmlWriteOptions
  case P.runPure asHtml of
    Left  err      -> H.span (H.toHtml $ show @String err)
    Right htmlText -> H.span (H.preEscapedToHtml htmlText)

latex_ :: Text -> H.Html
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

-- | Add headers for using Tufte css
tufteSetup :: H.Html
tufteSetup = do
  H.link ! HA.rel "stylesheet" ! HA.href
    "https://cdnjs.cloudflare.com/ajax/libs/tufte-css/1.4/tufte.min.css"
  H.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1"

-- | Wrap given html in appropriate headers for the hvega and latex functions to work
makeReportHtml :: Text -> H.Html -> H.Html
makeReportHtml title reportHtml = H.html $ H.docTypeHtml $ do
  H.head $ do
    H.title (H.toHtml title)
    tufteSetup
    mathJaxScript
    vegaScripts2
  H.body $ H.article reportHtml

-- | Add an hvega visualization with the given id
placeVisualization :: Text -> GV.VegaLite -> H.Html
placeVisualization idText vl =
  let vegaScript :: Text =
        decodeUtf8 $ BS.toStrict $ A.encodePretty $ GV.fromVL vl
      script =
        "var vlSpec=\n"
          <> vegaScript
          <> ";\n"
          <> "vegaEmbed(\'#"
          <> idText
          <> "\',vlSpec);"
  in  H.figure
      ! HA.id (H.toValue idText)
      $ H.script
      ! HA.type_ "text/javascript"
      $ H.preEscapedToHtml script

-- | Add the given Html as a new section
placeTextSection :: H.Html -> H.Html
placeTextSection = H.section

