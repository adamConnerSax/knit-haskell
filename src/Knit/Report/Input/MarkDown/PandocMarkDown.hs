{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-|
Module      : Knit.Report.Input.Markdown.PandocMarkdown
Description : Functions to add Pandoc MarkDown fragments to a Pandoc
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to add Pandoc MarkDown fragments to the current Pandoc.
-}
module Knit.Report.Input.MarkDown.PandocMarkDown
  (
    -- * Default Options
    markDownReaderOptions

    -- * functions to add various thing to the current Pandoc
  , addMarkDown
  , addMarkDownWithOptions
  )
where

import qualified Data.Text                     as T
import qualified Text.Pandoc                   as PA

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc           as PE
import qualified Knit.Effect.PandocMonad      as PM

-- | Base Pandoc MarkDown reader options
markDownReaderOptions :: PA.ReaderOptions
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

-- | Add a Pandoc MarkDown fragment with the given options
addMarkDownWithOptions
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => PA.ReaderOptions
  -> T.Text
  -> P.Semantic effs ()
addMarkDownWithOptions opts = PE.addFrom PE.ReadMarkDown opts

-- | Add a Pandoc MarkDown fragment with default options
addMarkDown 
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> P.Semantic effs ()
addMarkDown = addMarkDownWithOptions markDownReaderOptions

