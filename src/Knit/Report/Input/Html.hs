{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-|
Module      : Knit.Report.Input.Html
Description : Support functions for adding Html fragments into a Pandoc document.
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Functions and Pandoc option sets for adding Html fragments into a Pandoc document. 
-}
module Knit.Report.Input.Html
  (
    -- * Default Options
    htmlReaderOptions
  , htmlReaderOptionsWithHeader
  -- * functions to add html formatted as Text to the current Pandoc
  , addStrictTextHtml
  , addLazyTextHtml
  )
where

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Text.Pandoc        as P
import qualified Text.Pandoc.Extensions        as PA
import qualified Lucid                         as LH

import qualified Polysemy                      as P
import           Polysemy                       ( Member
                                                , Semantic
                                                )
import qualified Knit.Effects.Pandoc           as PE
import qualified Knit.Effects.PandocMonad      as PM


-- | Base Html reader options 
htmlReaderOptions =
  P.def { P.readerExtensions = PA.extensionsFromList [PA.Ext_raw_html] }

-- | Html reader options for complete document
htmlReaderOptionsWithHeader = htmlReaderOptions { P.readerStandalone = True }

-- | add Strict Text Html to current Pandoc
addStrictTextHtml
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> P.Semantic effs ()
addStrictTextHtml = PE.addFrom PE.ReadHtml htmlReaderOptions

-- | add Lazy Text Html to current Pandoc
addLazyTextHtml
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => LT.Text
  -> P.Semantic effs ()
addLazyTextHtml = addStrictTextHtml . LT.toStrict 
