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
module Text.Pandoc.Report
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
  -- * converters
  , markDownTextToBlazeFragment
  -- * formatted output
  , toBlazeDocument
  , pandocWriterToBlazeDocument
  -- * options helper
  , mindocOptionsF
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
import qualified Text.Pandoc                   as P
import qualified Text.Pandoc.Extensions        as P
import qualified Lucid                         as LH
import qualified Control.Monad.Freer.Pandoc    as P
import qualified Control.Monad.Freer.PandocMonad
                                               as PM
import qualified Control.Monad.Freer.Html      as H
import qualified Control.Monad.Freer           as FR

-- | Base Html reader options 
htmlReaderOptions =
  P.def { P.readerExtensions = P.extensionsFromList [P.Ext_raw_html] }

-- | Html reader options for complete document
htmlReaderOptionsWithHeader = htmlReaderOptions { P.readerStandalone = True }

-- | Base Html writer options, with support for MathJax
htmlWriterOptions = P.def
  { P.writerExtensions     = P.extensionsFromList [P.Ext_raw_html]
  , P.writerHTMLMathMethod = P.MathJax ""
  }

-- | Full writer options which use pandoc monad for template access
htmlFullDocWriterOptions
  :: P.PandocMonad m
  => Maybe FilePath
  -> M.Map String String
  -> m P.WriterOptions
htmlFullDocWriterOptions pathM templateVars = do
  template <- case pathM of
    Nothing -> P.getDefaultTemplate "Html5"
    Just fp -> do
      exists <- P.fileExists fp
      if exists
        then fmap BS.unpack (P.readFileStrict fp)
        else P.logOutput (P.IgnoredIOError ("Couldn't find " ++ show fp))
          >> P.getDefaultTemplate "Html5"
  return $ htmlWriterOptions { P.writerTemplate      = Just template
                             , P.writerVariables     = M.toList templateVars
                             , P.writerSetextHeaders = True
                             }

-- | Base markdown reader options
markDownReaderOptions = P.def
  { P.readerStandalone = True
  , P.readerExtensions = P.extensionsFromList
                           [ P.Ext_auto_identifiers
                           , P.Ext_backtick_code_blocks
                           , P.Ext_fancy_lists
                           , P.Ext_footnotes
                           , P.Ext_simple_tables
                           , P.Ext_multiline_tables
                           , P.Ext_tex_math_dollars
                           , P.Ext_header_attributes
                           , P.Ext_implicit_header_references
                           ]
  }

-- | add a markdown bit with default options
addMarkDown
  :: (PM.PandocEffects effs, FR.Member P.ToPandoc effs)
  => T.Text
  -> FR.Eff effs ()
addMarkDown = P.addFrom P.ReadMarkDown markDownReaderOptions

-- | add Text formatted as Html
addHtml
  :: (PM.PandocEffects effs, FR.Member P.ToPandoc effs)
  => T.Text
  -> FR.Eff effs ()
addHtml = P.addFrom P.ReadHtml htmlReaderOptions

-- | add Blaze Html 
addBlaze
  :: (PM.PandocEffects effs, FR.Member P.ToPandoc effs)
  => BH.Html
  -> FR.Eff effs ()
addBlaze = addHtml . LT.toStrict . BH.renderHtml

-- | add Lucid Html
addLucid
  :: (PM.PandocEffects effs, FR.Member P.ToPandoc effs)
  => LH.Html ()
  -> FR.Eff effs ()
addLucid = addHtml . LT.toStrict . LH.renderText

-- | Convert markDown to Blaze
markDownTextToBlazeFragment
  :: PM.PandocEffects effs => T.Text -> FR.Eff effs BH.Html
markDownTextToBlazeFragment =
  P.fromPandocE P.WriteHtml5 htmlWriterOptions
    . P.addFrom P.ReadMarkDown markDownReaderOptions

-- | Convert given Pandoc to Blaze Html.
-- Incudes support for template and template variables and changes to the default writer options
toBlazeDocument
  :: PM.PandocEffects effs
  => Maybe FilePath
  -> M.Map String String
  -> (P.WriterOptions -> P.WriterOptions)
  -> P.Pandoc
  -> FR.Eff effs BH.Html
toBlazeDocument templatePathM templateVars optionsF pdoc = do
  writerOptions <- htmlFullDocWriterOptions templatePathM templateVars
  P.fromPandoc P.WriteHtml5 (optionsF writerOptions) pdoc

-- | Convert current Pandoc document (from the ToPandoc effect) into a Blaze Html document.
-- Incudes support for template and template variables and changes to the default writer options. 
pandocWriterToBlazeDocument
  :: PM.PandocEffects effs
  => Maybe FilePath
  -> M.Map String String
  -> (P.WriterOptions -> P.WriterOptions)
  -> FR.Eff (P.ToPandoc ': effs) ()
  -> FR.Eff effs BH.Html
pandocWriterToBlazeDocument templatePathM templateVars optionsF pw =
  P.runPandocWriter pw >>= toBlazeDocument templatePathM templateVars optionsF

-- | options for the mindoc template 
mindocOptionsF op = op { P.writerSectionDivs = True }
