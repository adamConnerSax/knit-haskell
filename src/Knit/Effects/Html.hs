{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE GADTs               #-}
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
module Knit.Effects.Html
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

import qualified Polysemy                      as P
import qualified Polysemy.Writer               as P

import qualified Lucid                         as LH
import qualified Text.Blaze.Html               as BH
--import qualified Text.Blaze.Html.Renderer.Pretty as BH
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                     as T

import           Knit.Effects.Docs              ( Docs
                                                , NamedDoc(..)
                                                , newDoc
                                                , toNamedDocList
                                                )

-- For now, just handle the Html () case since then it's monoidal and we can interpret via writer
--newtype FreerHtml = FreerHtml { unFreer :: H.Html () }

-- | type-alias for a single Lucid document writer
type Lucid = P.Writer (LH.Html ())

-- | type-alias for a single Blaze document writer
type Blaze = P.Writer BH.Html

-- | combinator for adding Lucid html to the current Lucid doc
lucid :: P.Member Lucid effs => LH.Html () -> P.Semantic effs ()
lucid = P.tell

-- | combinator for adding Blaze html to the current Blaze doc
blaze :: P.Member Blaze effs => BH.Html -> P.Semantic effs ()
blaze = P.tell

-- | type-alias for the Docs effect specialized to Lucid docs.  To be used in an app that produces multiple html outputs, built up from Lucid bits.
type LucidDocs = Docs (LH.Html ())

-- | type-alias for the Docs effect specialized to Blaze docs.  To be used in an app that produces multiple html outputs, built up from Blaze bits.
type BlazeDocs = Docs BH.Html

--newHtmlDocPure :: FR.Member HtmlDocs effs => T.Text -> H.Html () -> FR.Eff effs ()
--newHtmlDocPure = newDoc  
-- | take the current Lucid html in the writer and add it to the set of named docs with the given name
newLucidDoc
  :: P.Member LucidDocs effs
  => T.Text
  -> P.Semantic (Lucid ': effs) ()
  -> P.Semantic effs ()
newLucidDoc n l = (fmap fst $ P.runWriter l) >>= newDoc n

-- | take the current Blaze html in the writer and add it to the set of named docs with the given name
newBlazeDoc
  :: P.Member BlazeDocs effs
  => T.Text
  -> P.Semantic (Blaze ': effs) ()
  -> P.Semantic effs ()
newBlazeDoc n l = (fmap fst $ P.runWriter l) >>= newDoc n

-- | run the LucidDocs effect, producing a list of names Lucid docs, suitable for writing to disk
lucidToNamedText
  :: P.Semantic (LucidDocs ': effs) () -> P.Semantic effs [NamedDoc TL.Text]
lucidToNamedText = fmap (fmap (fmap LH.renderText)) . toNamedDocList -- monad, list, NamedDoc itself

-- | run the BlazeDocs effect, producing a list of names Blaze docs
blazeToNamedText
  :: P.Semantic (BlazeDocs ': effs) () -> P.Semantic effs [NamedDoc TL.Text]
blazeToNamedText = fmap (fmap (fmap BH.renderHtml)) . toNamedDocList -- monad, list, NamedDoc itself

-- | run the Lucid effect, producing a Lucid @Html @() from the currently written doc
lucidHtml :: P.Semantic (Lucid ': effs) () -> P.Semantic effs (LH.Html ())
lucidHtml = fmap fst . P.runWriter

-- | run the Lucid effect, producing Text from the currently written doc
lucidToText :: P.Semantic (Lucid ': effs) () -> P.Semantic effs TL.Text
lucidToText = fmap LH.renderText . lucidHtml

-- | run the Blaze effect, producing a Blaze @Html from the currently written doc
blazeHtml :: P.Semantic (Blaze ': effs) () -> P.Semantic effs BH.Html
blazeHtml = fmap fst . P.runWriter

-- | run the Blaze effect, producing Text from the currently written doc
blazeToText :: P.Semantic (Blaze ': effs) () -> P.Semantic effs TL.Text
blazeToText = fmap BH.renderHtml . blazeHtml

