{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-|
Module      : Knit.Report.Output.Html
Description : Output Pandoc as Html
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to support some simple reports using Blaze.  Particularly to support adding latex and hvega charts.
-}
module Knit.Report.Output.Html
  (
    -- * Default Options
    htmlWriterOptions
    -- * formatted output
  , toBlazeDocument
  , pandocWriterToBlazeDocument
  -- * options helper  
  , mindocOptionsF
  )
where


import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Map                      as M
import qualified Graphics.Vega.VegaLite        as GV
import qualified Text.Blaze.Html               as BH
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Text.Pandoc                   as PA
import qualified Text.Pandoc.Extensions        as PA
import qualified Lucid                         as LH

import qualified Polysemy                      as P
import           Polysemy                       ( Member
                                                , Semantic
                                                )
import qualified Knit.Effects.Pandoc           as PE
import qualified Knit.Effects.PandocMonad      as PM
import qualified Knit.Effects.Html             as H

import           Knit.Report.Input.MarkDown.PandocMarkDown (markDownReaderOptions)
{-
import           Knit.Report.Other.Blaze              ( latexToHtml
                                                      , placeVisualization
                                                      )
-}

-- | Base Html writer options, with support for MathJax
htmlWriterOptions = PA.def
  { PA.writerExtensions     = PA.extensionsFromList [PA.Ext_raw_html]
  , PA.writerHTMLMathMethod = PA.MathJax ""
  }

-- | Full writer options which use pandoc monad for template access
htmlFullDocWriterOptions
  :: PA.PandocMonad m
  => Maybe FilePath
  -> M.Map String String
  -> m PA.WriterOptions
htmlFullDocWriterOptions pathM templateVars = do
  template <- case pathM of
    Nothing -> PA.getDefaultTemplate "Html5"
    Just fp -> do
      exists <- PA.fileExists fp
      if exists
        then fmap BS.unpack (PA.readFileStrict fp)
        else PA.logOutput (PA.IgnoredIOError ("Couldn't find " ++ show fp))
          >> PA.getDefaultTemplate "Html5"
  return $ htmlWriterOptions { PA.writerTemplate      = Just template
                             , PA.writerVariables     = M.toList templateVars
                             , PA.writerSetextHeaders = True
                             }


-- | Convert markDown to Blaze
markDownTextToBlazeFragment
  :: PM.PandocEffects effs => T.Text -> P.Semantic effs BH.Html
markDownTextToBlazeFragment =
  PE.fromPandocE PE.WriteHtml5 htmlWriterOptions
    . PE.addFrom PE.ReadMarkDown markDownReaderOptions

-- | Convert given Pandoc to Blaze Html.
-- Incudes support for template and template variables and changes to the default writer options
toBlazeDocument
  :: PM.PandocEffects effs
  => Maybe FilePath
  -> M.Map String String
  -> (PA.WriterOptions -> PA.WriterOptions)
  -> PE.PandocWithRequirements
  -> P.Semantic effs BH.Html
toBlazeDocument templatePathM templateVars optionsF pdocWR = do
  writerOptions <- htmlFullDocWriterOptions templatePathM templateVars
  PE.fromPandoc PE.WriteHtml5 (optionsF writerOptions) pdocWR

-- | Convert current Pandoc document (from the ToPandoc effect) into a Blaze Html document.
-- Incudes support for template and template variables and changes to the default writer options. 
pandocWriterToBlazeDocument
  :: PM.PandocEffects effs
  => Maybe FilePath
  -> M.Map String String
  -> (PA.WriterOptions -> PA.WriterOptions)
  -> P.Semantic (PE.ToPandoc ': effs) ()
  -> P.Semantic effs BH.Html
pandocWriterToBlazeDocument templatePathM templateVars optionsF pw =
  PE.runPandocWriter pw >>= toBlazeDocument templatePathM templateVars optionsF

-- | options for the mindoc template 
mindocOptionsF op = op { PA.writerSectionDivs = True }
