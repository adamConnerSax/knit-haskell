{-# LANGUAGE FlexibleContexts    #-}
--{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Control.Monad.Freer.Html
  ( Lucid
  , Blaze
  , LucidDocs
  , BlazeDocs
  , NamedDoc(..)
  , lucid
  , blaze
  , lucidToNamedText
  , blazeToNamedText
  , lucidHtml
  , lucidToText
  , blazeHtml
  , blazeToText
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

type Lucid = FR.Writer (LH.Html ())
type Blaze = FR.Writer BH.Html

lucid :: FR.Member Lucid effs => LH.Html () -> FR.Eff effs ()
lucid = FR.tell

blaze :: FR.Member Blaze effs => BH.Html -> FR.Eff effs ()
blaze = FR.tell

type LucidDocs = Docs (LH.Html ())
type BlazeDocs = Docs BH.Html

--newHtmlDocPure :: FR.Member HtmlDocs effs => T.Text -> H.Html () -> FR.Eff effs ()
--newHtmlDocPure = newDoc  

newLucidDoc
  :: FR.Member LucidDocs effs
  => T.Text
  -> FR.Eff (Lucid ': effs) ()
  -> FR.Eff effs ()
newLucidDoc n l = (fmap snd $ FR.runWriter l) >>= newDoc n

newBlazeDoc
  :: FR.Member BlazeDocs effs
  => T.Text
  -> FR.Eff (Blaze ': effs) ()
  -> FR.Eff effs ()
newBlazeDoc n l = (fmap snd $ FR.runWriter l) >>= newDoc n

lucidToNamedText
  :: FR.Eff (LucidDocs ': effs) () -> FR.Eff effs [NamedDoc TL.Text]
lucidToNamedText = fmap (fmap (fmap LH.renderText)) . toNamedDocList -- monad, list, NamedDoc itself

blazeToNamedText
  :: FR.Eff (BlazeDocs ': effs) () -> FR.Eff effs [NamedDoc TL.Text]
blazeToNamedText = fmap (fmap (fmap BH.renderHtml)) . toNamedDocList -- monad, list, NamedDoc itself

lucidHtml :: FR.Eff (Lucid ': effs) () -> FR.Eff effs (LH.Html ())
lucidHtml = fmap snd . FR.runWriter

lucidToText :: FR.Eff (Lucid ': effs) () -> FR.Eff effs TL.Text
lucidToText = fmap LH.renderText . lucidHtml

blazeHtml :: FR.Eff (Blaze ': effs) () -> FR.Eff effs BH.Html
blazeHtml = fmap snd . FR.runWriter

blazeToText :: FR.Eff (Blaze ': effs) () -> FR.Eff effs TL.Text
blazeToText = fmap BH.renderHtml . blazeHtml

