{-# LANGUAGE ExtendedDefaultRules #-}
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
    -- * Add html formatted as Text
    addStrictTextHtml
  , addLazyTextHtml

    -- * Default Options
  , htmlReaderOptions
  , htmlReaderOptionsWithHeader
  )
where

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Text.Pandoc                   as P
import qualified Text.Pandoc.Extensions        as PA

import qualified Polysemy                      as P
import qualified Knit.Effect.Pandoc            as PE
import qualified Knit.Effect.PandocMonad       as PM


-- | Base Html reader options
htmlReaderOptions :: P.ReaderOptions
htmlReaderOptions =
  P.def { P.readerExtensions = PA.extensionsFromList [PA.Ext_raw_html] }

-- | Html reader options for complete document
htmlReaderOptionsWithHeader :: P.ReaderOptions
htmlReaderOptionsWithHeader = htmlReaderOptions { P.readerStandalone = True }

-- | Add Strict Text Html to current Pandoc
addStrictTextHtml
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => T.Text
  -> P.Sem effs ()
addStrictTextHtml = PE.addFrom PE.ReadHtml htmlReaderOptions

-- | Add Lazy Text Html to current Pandoc
addLazyTextHtml
  :: (PM.PandocEffects effs, P.Member PE.ToPandoc effs)
  => LT.Text
  -> P.Sem effs ()
addLazyTextHtml = addStrictTextHtml . toText
