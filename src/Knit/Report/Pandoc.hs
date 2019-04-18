{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-|
Module      : Text.Pandoc.Report
Description : Support functions for simple reports using Pandoc
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to support some simple reports using Blaze.  Particularly to support adding latex and hvega charts.
-}
module Knit.Report.Pandoc
  (
    -- * Default Options
    htmlReaderOptions
  , htmlReaderOptionsWithHeader
  , htmlWriterOptions
  , markDownReaderOptions
  -- * functions to add various thing to the current Pandoc
  , addMarkDown
  , addHtml
  , addBlaze
  , addLucid
  , addLatex
  , addHvega
  -- * converters
  , markDownTextToBlazeFragment
  -- * formatted output
  , toBlazeDocument
  , pandocWriterToBlazeDocument
  -- * options helper  
  , mindocOptionsF
  -- * re-exports
  , Member
  , Semantic
  )
where

import           Control.Monad.Trans            ( liftIO )
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
import           Knit.Report.Blaze              ( latexToHtml
                                                , placeVisualization
                                                )

-- | Base Html reader options 
htmlReaderOptions =
  PA.def { PA.readerExtensions = PA.extensionsFromList [PA.Ext_raw_html] }

-- | Html reader options for complete document
htmlReaderOptionsWithHeader = htmlReaderOptions { PA.readerStandalone = True }

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

-- | Base markdown reader options
markDownReaderOptions = PA.def
  { PA.readerStandalone = True
  , PA.readerExtensions = PA.extensionsFromList
                            [ PA.Ext_auto_identifiers
                            , PA.Ext_backtick_code_blocks
                            , PA.Ext_fancy_lists
                            , PA.Ext_footnotes
                            , PA.Ext_simple_tables
                            , PA.Ext_multiline_tables
                            , PA.Ext_tex_math_dollars
                            , PA.Ext_header_attributes
                            , PA.Ext_implicit_header_references
                            ]
  }

-- | add a markdown bit with default options
addMarkDown
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> P.Semantic effs ()
addMarkDown = PE.addFrom PE.ReadMarkDown markDownReaderOptions

-- | add Text formatted as Html
addHtml
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> P.Semantic effs ()
addHtml = PE.addFrom PE.ReadHtml htmlReaderOptions

-- | add Blaze Html 
addBlaze
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => BH.Html
  -> P.Semantic effs ()
addBlaze = addHtml . LT.toStrict . BH.renderHtml

-- | add Lucid Html
addLucid
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => LH.Html ()
  -> P.Semantic effs ()
addLucid = addHtml . LT.toStrict . LH.renderText

-- | add latex (via blaze)
addLatex
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> P.Semantic effs ()
addLatex = addBlaze . latexToHtml

-- | add hvega (via blaze)
addHvega
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> GV.VegaLite
  -> P.Semantic effs ()
addHvega vizId = addBlaze . placeVisualization vizId


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
  -> PA.Pandoc
  -> P.Semantic effs BH.Html
toBlazeDocument templatePathM templateVars optionsF pdoc = do
  writerOptions <- htmlFullDocWriterOptions templatePathM templateVars
  PE.fromPandoc PE.WriteHtml5 (optionsF writerOptions) pdoc

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
