{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Control.Monad.Freer.Pandoc
Description : freer-simple logging effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

freer-simple Pandoc effect.  This is writer-like, allowing the interspersed addition of various Pandoc-readable formats into one doc and then rendering
to many Pandoc-writeable formats.  Currently only a subset of formats are supported.
-}
module Control.Monad.Freer.Pandoc
  (
    -- * Effect Types
    ToPandoc
  , FromPandoc
  -- * Format ADTs
  , PandocReadFormat(..)
  , PandocWriteFormat(..)
  -- * Combinators
  , addFrom
  , writeTo
  , toPandoc
  , fromPandoc
  -- * run the effect to produce a document
  , runPandocWriter
  -- * Docs effect type-aliases
  , Pandocs
  , NamedDoc(..)
  -- * use with the Docs effect 
  , newPandoc
  , pandocsToNamed
  , fromPandocE
  )
where

import qualified Text.Pandoc                   as P
import qualified Data.Text                     as T
import           Data.ByteString.Lazy          as LBS
import qualified Text.Blaze.Html               as Blaze
import qualified Control.Monad.Freer           as FR
import qualified Control.Monad.Freer.PandocMonad
                                               as FR
import qualified Control.Monad.Freer.Writer    as FR
import           Control.Monad.Freer.Docs       ( Docs
                                                , NamedDoc(..)
                                                , newDoc
                                                , toNamedDocList
                                                )

-- For now, just handle the Html () case since then it's monoidal and we can interpret via writer
--newtype FreerHtml = FreerHtml { unFreer :: H.Html () }
-- | Supported formats for adding to current Pandoc
data PandocReadFormat a where
  ReadDocX :: PandocReadFormat LBS.ByteString
  ReadMarkDown :: PandocReadFormat T.Text
  ReadCommonMark :: PandocReadFormat T.Text
  ReadRST :: PandocReadFormat T.Text
  ReadLaTeX :: PandocReadFormat T.Text
  ReadHtml :: PandocReadFormat T.Text

deriving instance Show (PandocReadFormat a)

-- | Supported formats for writing current Pandoc
data PandocWriteFormat a where
  WriteDocX :: PandocWriteFormat LBS.ByteString
  WriteMarkDown :: PandocWriteFormat T.Text
  WriteCommonMark :: PandocWriteFormat T.Text
  WriteRST :: PandocWriteFormat T.Text
  WriteLaTeX :: PandocWriteFormat T.Text
  WriteHtml5 :: PandocWriteFormat Blaze.Html -- Blaze
  WriteHtml5String :: PandocWriteFormat T.Text

deriving instance Show (PandocWriteFormat a)

-- | Pandoc writer, add any read format to current doc
data ToPandoc r where
  AddFrom  :: PandocReadFormat a -> P.ReaderOptions -> a -> ToPandoc () -- add to current doc


-- | Pandoc output effect, take given doc and produce formatted output
data FromPandoc r where
  WriteTo  :: PandocWriteFormat a -> P.WriterOptions -> P.Pandoc -> FromPandoc a -- convert to given format


-- | add a piece of a Pandoc readable type to the current doc
addFrom
  :: FR.Member ToPandoc effs
  => PandocReadFormat a
  -> P.ReaderOptions
  -> a
  -> FR.Eff effs ()
addFrom prf pro doc = FR.send $ AddFrom prf pro doc

-- | write given doc in requested format
writeTo
  :: FR.Member FromPandoc effs
  => PandocWriteFormat a
  -> P.WriterOptions
  -> P.Pandoc
  -> FR.Eff effs a
writeTo pwf pwo pdoc = FR.send $ WriteTo pwf pwo pdoc

-- | Convert a to Pandoc with the given options
toPandoc
  :: P.PandocMonad m => PandocReadFormat a -> P.ReaderOptions -> a -> m P.Pandoc
toPandoc prf pro x = read pro x
 where
  read = case prf of
    ReadDocX       -> P.readDocx
    ReadMarkDown   -> P.readMarkdown
    ReadCommonMark -> P.readCommonMark
    ReadRST        -> P.readRST
    ReadLaTeX      -> P.readLaTeX
    ReadHtml       -> P.readHtml

-- | convert Pandoc to a with the given options
fromPandoc
  :: P.PandocMonad m
  => PandocWriteFormat a
  -> P.WriterOptions
  -> P.Pandoc
  -> m a
fromPandoc pwf pwo pdoc = write pwo pdoc
 where
  write = case pwf of
    WriteDocX        -> P.writeDocx
    WriteMarkDown    -> P.writeMarkdown
    WriteCommonMark  -> P.writeCommonMark
    WriteRST         -> P.writeRST
    WriteLaTeX       -> P.writeLaTeX
    WriteHtml5       -> P.writeHtml5
    WriteHtml5String -> P.writeHtml5String

-- | Re-interpret ToPandoc in Writer
toWriter
  :: FR.PandocEffects effs
  => FR.Eff (ToPandoc ': effs) a
  -> FR.Eff (FR.Writer P.Pandoc ': effs) a
toWriter =
  FR.reinterpret (\(AddFrom rf ro x) -> FR.raise (toPandoc rf ro x) >>= FR.tell)

-- | run ToPandoc by interpreting in Writer and running that
runPandocWriter
  :: FR.PandocEffects effs
  => FR.Eff (ToPandoc ': effs) ()
  -> FR.Eff effs P.Pandoc
runPandocWriter = fmap snd . FR.runWriter . toWriter

-- | type-alias for use with the @Docs@ effect
type Pandocs = Docs P.Pandoc

-- | add a new named Pandoc to a Pandoc Docs collection
newPandocPure :: FR.Member Pandocs effs => T.Text -> P.Pandoc -> FR.Eff effs ()
newPandocPure = newDoc

-- | add the Pandoc stored in the writer-style ToPandoc effect to the named docs collection with the given name
newPandoc
  :: (FR.PandocEffects effs, FR.Member Pandocs effs)
  => T.Text
  -> FR.Eff (ToPandoc ': effs) ()
  -> FR.Eff effs ()
newPandoc n l = fmap snd (FR.runWriter $ toWriter l) >>= newPandocPure n

-- | Given a write format and options, convert the NamedDoc to the requested format
namedPandocFrom
  :: P.PandocMonad m
  => PandocWriteFormat a
  -> P.WriterOptions
  -> NamedDoc P.Pandoc
  -> m (NamedDoc a)
namedPandocFrom pwf pwo (NamedDoc n pdoc) = do
  doc <- fromPandoc pwf pwo pdoc
  return $ NamedDoc n doc

-- | Given a write format and options, convert a list of named Pandocs to a list of named docs in the requested format
pandocsToNamed
  :: FR.PandocEffects effs
  => PandocWriteFormat a
  -> P.WriterOptions
  -> FR.Eff (Pandocs ': effs) ()
  -> FR.Eff effs [NamedDoc a]
pandocsToNamed pwf pwo =
  (traverse (namedPandocFrom pwf pwo) =<<) . toNamedDocList -- monad, list, NamedDoc itself

-- | Given a write format and options, run the writer-style ToPandoc effect and produce a doc of requested type
fromPandocE
  :: FR.PandocEffects effs
  => PandocWriteFormat a
  -> P.WriterOptions
  -> FR.Eff (ToPandoc ': effs) ()
  -> FR.Eff effs a
fromPandocE pwf pwo =
  ((fromPandoc pwf pwo . snd) =<<) . FR.runWriter . toWriter

