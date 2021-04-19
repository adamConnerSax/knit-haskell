{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-|
Module      : Knit.Report.Input.RST
Description : Functions to RST fragments to a Pandoc
Copyright   : (c) Adam Conner-Sax 2021
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions to add RST fragments to the current Pandoc.
-}
module Knit.Report.Input.RST
  (
    -- * Default Options
    rstReaderOptions

    -- * functions to add various thing to the current Pandoc
  , addRST
  , addRSTWithOptions
  )
where

import qualified Data.Text                     as T
import qualified Text.Pandoc                   as PA

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM

-- | Base Pandoc MarkDown reader options
rstReaderOptions :: PA.ReaderOptions
rstReaderOptions = PA.def
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
                            , PA.Ext_implicit_figures
                            ]
  }

-- | Add a Pandoc RST fragment with the given options
addRSTWithOptions
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => PA.ReaderOptions
  -> T.Text
  -> P.Sem effs ()
addRSTWithOptions = PE.addFrom PE.ReadRST

-- | Add a Pandoc RST fragment with default options
addRST
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> P.Sem effs ()
addRST = addRSTWithOptions rstReaderOptions
