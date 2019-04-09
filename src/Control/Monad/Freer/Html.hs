{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Control.Monad.Freer.Html
Description : freer-simple writer effects for creating Lucid/Blaze documents
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Create a Lucid or Blaze html document (using a Writer to intersperse html and other code) and then use the Docs effect
to store that document for processing/output later.
-}
module Control.Monad.Freer.Html
  (
    -- * Lucid Types
    Lucid
  , LucidDocs
  -- * Lucid combinators
  , lucid
  , lucidToNamedText
  , lucidHtml
  , lucidToText
  -- * Blaze Types
  , Blaze
  , BlazeDocs
  -- * Blaze combinators
  , blaze
  , blazeToNamedText
  , blazeHtml
  , blazeToText
  -- * re-exports
  , NamedDoc(..)
  )
where

import qualified Lucid                         as LH
import qualified Text.Blaze.Html               as BH
--import qualified Text.Blaze.Html.Renderer.Pretty as BH
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                     as T
import qualified Control.Monad.Freer           as FR
import qualified Control.Monad.Freer.Writer    as FR
import           Control.Monad.Freer.Docs       ( Docs
                                                , NamedDoc(..)
                                                , newDoc
                                                , toNamedDocList
                                                )

-- For now, just handle the Html () case since then it's monoidal and we can interpret via writer
--newtype FreerHtml = FreerHtml { unFreer :: H.Html () }

-- | type-alias for a single Lucid document writer
type Lucid = FR.Writer (LH.Html ())

-- | type-alias for a single Blaze document writer
type Blaze = FR.Writer BH.Html

-- | combinator for adding Lucid html to the current Lucid doc
lucid :: FR.Member Lucid effs => LH.Html () -> FR.Eff effs ()
lucid = FR.tell

-- | combinator for adding Blaze html to the current Blaze doc
blaze :: FR.Member Blaze effs => BH.Html -> FR.Eff effs ()
blaze = FR.tell

-- | type-alias for the Docs effect specialized to Lucid docs.  To be used in an app that produces multiple html outputs, built up from Lucid bits.
type LucidDocs = Docs (LH.Html ())

-- | type-alias for the Docs effect specialized to Blaze docs.  To be used in an app that produces multiple html outputs, built up from Blaze bits.
type BlazeDocs = Docs BH.Html

--newHtmlDocPure :: FR.Member HtmlDocs effs => T.Text -> H.Html () -> FR.Eff effs ()
--newHtmlDocPure = newDoc  
-- | take the current Lucid html in the writer and add it to the set of named docs with the given name
newLucidDoc
  :: FR.Member LucidDocs effs
  => T.Text
  -> FR.Eff (Lucid ': effs) ()
  -> FR.Eff effs ()
newLucidDoc n l = (fmap snd $ FR.runWriter l) >>= newDoc n

-- | take the current Blaze html in the writer and add it to the set of named docs with the given name
newBlazeDoc
  :: FR.Member BlazeDocs effs
  => T.Text
  -> FR.Eff (Blaze ': effs) ()
  -> FR.Eff effs ()
newBlazeDoc n l = (fmap snd $ FR.runWriter l) >>= newDoc n

-- | run the LucidDocs effect, producing a list of names Lucid docs, suitable for writing to disk
lucidToNamedText
  :: FR.Eff (LucidDocs ': effs) () -> FR.Eff effs [NamedDoc TL.Text]
lucidToNamedText = fmap (fmap (fmap LH.renderText)) . toNamedDocList -- monad, list, NamedDoc itself

-- | run the BlazeDocs effect, producing a list of names Blaze docs
blazeToNamedText
  :: FR.Eff (BlazeDocs ': effs) () -> FR.Eff effs [NamedDoc TL.Text]
blazeToNamedText = fmap (fmap (fmap BH.renderHtml)) . toNamedDocList -- monad, list, NamedDoc itself

-- | run the Lucid effect, producing a Lucid @Html @() from the currently written doc
lucidHtml :: FR.Eff (Lucid ': effs) () -> FR.Eff effs (LH.Html ())
lucidHtml = fmap snd . FR.runWriter

-- | run the Lucid effect, producing Text from the currently written doc
lucidToText :: FR.Eff (Lucid ': effs) () -> FR.Eff effs TL.Text
lucidToText = fmap LH.renderText . lucidHtml

-- | run the Blaze effect, producing a Blaze @Html from the currently written doc
blazeHtml :: FR.Eff (Blaze ': effs) () -> FR.Eff effs BH.Html
blazeHtml = fmap snd . FR.runWriter

-- | run the Blaze effect, producing Text from the currently written doc
blazeToText :: FR.Eff (Blaze ': effs) () -> FR.Eff effs TL.Text
blazeToText = fmap BH.renderHtml . blazeHtml

