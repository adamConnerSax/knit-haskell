{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-|
Module      : Knit.Report.Input.Visualization.Hvega
Description : Support functions for simple reports using Pandoc
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to add hvega charts (using Blaze Html) to the current Pandoc document.
-}
module Knit.Report.Input.Visualization.Hvega
  (
    -- * Add hvega Inputs
    addHvega
  )
where

import           Knit.Report.Input.Html.Blaze   ( addBlaze )

import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.ByteString.Lazy.Char8    as BS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Graphics.Vega.VegaLite        as GV
import qualified Text.Blaze.Html5              as BH
import qualified Text.Blaze.Html5.Attributes   as BHA

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM


-- TODO: Add some autogenerated unique id support

-- | Add hvega (via html). Requires html since vega-lite renders using javascript.
addHvega
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> GV.VegaLite
  -> P.Semantic effs ()
addHvega vizId vl = do
  PE.require PE.VegaSupport
  addBlaze $ placeVisualization vizId vl


-- | Build (Blaze) Html for  hvega visualization with the given id
placeVisualization :: T.Text -> GV.VegaLite -> BH.Html
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
  in  BH.figure
      BH.! BHA.id (BH.toValue idText)
      $    BH.script
      BH.! BHA.type_ "text/javascript"
      $    BH.preEscapedToHtml script






