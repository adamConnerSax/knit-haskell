{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE PolyKinds                     #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.Html
Description : Polysemy writer effects for creating Lucid/Blaze documents
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Create a Lucid or Blaze html document (using a Writer to intersperse html and other code) and then use the 'Knit.Haskell.Docs' <https://github.com/isovector/polysemy#readme polysemy> effect
to store that document for processing/output later.
-}
module Knit.Effect.Html
  (
    -- * Lucid

    -- ** Effects
    Lucid
  , LucidDocs

    -- ** Actions
  , lucid

    -- ** Intepretations
  , lucidToNamedText
  , lucidHtml
  , lucidToText
  , newLucidDoc

  -- * Blaze

  -- ** Effects
  , Blaze
  , BlazeDocs

  -- ** Actions
  , blaze

  -- ** Interpretations
  , blazeToNamedText
  , blazeHtml
  , blazeToText
  , newBlazeDoc

    -- * Re-exports
  , NamedDoc(..)
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.Writer               as P

import qualified Lucid                         as LH
import qualified Text.Blaze.Html               as BH
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                     as T

import           Knit.Effect.Docs               ( Docs
                                                , NamedDoc(..)
                                                , newDoc
                                                , toNamedDocList
                                                )

-- For now, just handle the Html () case since then it's monoidal and we can interpret via writer
--newtype FreerHtml = FreerHtml { unFreer :: H.Html () }

-- | Type-Alias for a single Lucid document writer.
type Lucid = P.Writer (LH.Html ())

-- | Type-Alias for a single Blaze document writer.
type Blaze = P.Writer BH.Html

-- | Add a Lucid html fragment to the current Lucid doc.
lucid :: P.Member Lucid effs => LH.Html () -> P.Sem effs ()
lucid = P.tell

-- | Add a Blaze html fragment to the current Blaze doc.
blaze :: P.Member Blaze effs => BH.Html -> P.Sem effs ()
blaze = P.tell

-- | Type-Alias for the 'Knit.Effects.Docs' effect (multi-document Writer), specialized to Lucid docs.
-- To be used in an app that produces multiple html outputs, built up from Lucid bits.
type LucidDocs = Docs (LH.Html ())

-- | Type-Alias for the 'Knit.Effects.Docs' effect (multi-document Writer) specialized to Blaze docs.
-- To be used in an app that produces multiple html outputs, built up from Blaze bits.
type BlazeDocs = Docs BH.Html

-- | Take the current Lucid HTML in the writer and add it to the set of named docs with the given name.
-- NB: Only use this function for making sets of documents built exclusively from Lucid.  Otherwise use the more general Pandoc infrastructure in
-- 'Knit.Effects.Pandoc'.
newLucidDoc
  :: P.Member LucidDocs effs
  => T.Text
  -> P.Sem (Lucid ': effs) ()
  -> P.Sem effs ()
newLucidDoc n l = (fmap fst $ P.runWriter l) >>= newDoc n

-- | take the current Blaze HTML in the writer and add it to the set of named docs with the given name
-- NB: Only use this function for making sets of documents built exclusively from Blaze. Otherwise use the more general Pandoc infrastructure in
-- 'Knit.Effects.Pandoc'.
newBlazeDoc
  :: P.Member BlazeDocs effs
  => T.Text
  -> P.Sem (Blaze ': effs) ()
  -> P.Sem effs ()
newBlazeDoc n l = (fmap fst $ P.runWriter l) >>= newDoc n

-- | Interpret the LucidDocs effect (via Writer), producing a list of named Lucid docs, suitable for writing to disk.
lucidToNamedText
  :: P.Sem (LucidDocs ': effs) () -> P.Sem effs [NamedDoc TL.Text]
lucidToNamedText = fmap (fmap (fmap LH.renderText)) . toNamedDocList -- monad, list, NamedDoc itself

-- | Interpret the BlazeDocs effect (via Writer), producing a list of named Blaze docs.
blazeToNamedText
  :: P.Sem (BlazeDocs ': effs) () -> P.Sem effs [NamedDoc TL.Text]
blazeToNamedText = fmap (fmap (fmap BH.renderHtml)) . toNamedDocList -- monad, list, NamedDoc itself

-- | Interprest the Lucid effect (via Writer), producing a Lucid @Html ()@ from the currently written doc
lucidHtml :: P.Sem (Lucid ': effs) () -> P.Sem effs (LH.Html ())
lucidHtml = fmap fst . P.runWriter

-- | Interpret the Lucid effect (via Writer), producing @Text@ from the currently written doc
lucidToText :: P.Sem (Lucid ': effs) () -> P.Sem effs TL.Text
lucidToText = fmap LH.renderText . lucidHtml

-- | Interpret the Blaze effect (via Writer), producing a Blaze @Html@ from the currently written doc.
blazeHtml :: P.Sem (Blaze ': effs) () -> P.Sem effs BH.Html
blazeHtml = fmap fst . P.runWriter

-- | Interpret the Blaze effect (via Writer), producing @Text@ from the currently written doc.
blazeToText :: P.Sem (Blaze ': effs) () -> P.Sem effs TL.Text
blazeToText = fmap BH.renderHtml . blazeHtml

