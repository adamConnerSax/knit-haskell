{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
Module      : Knit.Report.Other.Lucid
Description : freer-simple random effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to support some simple reports using Lucid.  Particularly to support adding latex and hvega charts.
-}
module Knit.Report.Other.Lucid
  (
    -- * Setup, headers, scripts, etc.
    makeReportHtml
    -- * add specific report bits
  , placeVisualization
  , placeTextSection
  , latexToHtml
  -- * helpers
  , latex_
  )
where

import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Graphics.Vega.VegaLite     as GV
import qualified Lucid                      as H
import qualified Text.Pandoc                as P

-- | Convert Latex to Lucid Html
latexToHtml :: T.Text -> H.Html ()
latexToHtml lText = do
  let latexReadOptions = P.def
      htmlWriteOptions = P.def { P.writerHTMLMathMethod = P.MathJax "" }
      asHtml = P.readLaTeX latexReadOptions lText >>= P.writeHtml5String htmlWriteOptions
  case P.runPure asHtml of
    Left err       -> H.span_ (H.toHtml $ show err)
    Right htmlText -> H.span_ (H.toHtmlRaw htmlText)

-- | Convert Latex to Lucid Html
latex_ :: T.Text -> H.Html ()
latex_ = latexToHtml

-- | Add headers for MathJax
mathJaxScript :: H.Html ()
mathJaxScript = H.script_ [H.src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML", H.async_ ""] ("" :: String)

-- | Add headers to use vega-lite (v2)
vegaScripts2 :: H.Html ()
vegaScripts2 = do
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega@4.4.0"] ("" :: String)
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega-lite@3.0.0-rc11"] ("" :: String)
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega-embed@3.28.0"] ("" :: String)

-- | Add headers to use vega-lite (v3)
vegaScripts3 :: H.Html ()
vegaScripts3 = do
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega@4.4.0/build/vega.js"] ("" :: String)
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega-lite@3.0.0-rc12/build/vega-lite.js"] ("" :: String)
  H.script_ [H.src_ "https://cdn.jsdelivr.net/npm/vega-embed@3.29.1/build/vega-embed.js"] ("" :: String)

-- | Add headers to use Tufte css
tufteSetup :: H.Html ()
tufteSetup = do
   H.link_ [H.rel_ "stylesheet", H.href_ "https://cdnjs.cloudflare.com/ajax/libs/tufte-css/1.4/tufte.min.css"]
   H.meta_ [H.name_ "viewport", H.content_"width=device-width, initial-scale=1"]

-- | -- | wrap given html in appropriate headers for the hvega and latex functions to work
makeReportHtml :: T.Text -> H.Html a -> H.Html a
makeReportHtml title reportHtml = H.html_ $ htmlHead >> H.body_ (H.article_ reportHtml) where
  htmlHead :: H.Html () = H.head_ (do
                                  H.title_ (H.toHtmlRaw title)
                                  tufteSetup
                                  mathJaxScript
                                  vegaScripts2
                                  return ()
                              )

-- | add an hvega visualization with the given id
placeVisualization :: T.Text -> GV.VegaLite -> H.Html ()
placeVisualization idText vl =
  let vegaScript :: T.Text = T.decodeUtf8 $ BS.toStrict $ A.encodePretty $ GV.fromVL vl
      script = "var vlSpec=\n" <> vegaScript <> ";\n" <> "vegaEmbed(\'#" <> idText <> "\',vlSpec);"
  in H.figure_ [H.id_ idText] (H.script_ [H.type_ "text/javascript"]  (H.toHtmlRaw script))

-- | add the given Html as a new section
placeTextSection :: H.Html () -> H.Html ()
placeTextSection x = H.section_ [{- attributes/styles here -}] x

