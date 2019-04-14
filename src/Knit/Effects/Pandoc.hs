{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
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
module Knit.Effects.Pandoc
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

import qualified Text.Pandoc                   as PA
import qualified Data.Text                     as T
import           Data.ByteString.Lazy          as LBS
import qualified Text.Blaze.Html               as Blaze

import qualified Polysemy                      as P
import           Polysemy.Internal              ( send )
import qualified Polysemy.Writer               as P


import qualified Knit.Effects.PandocMonad      as PM

import           Knit.Effects.Docs              ( Docs
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
data ToPandoc m r where
  AddFrom  :: PandocReadFormat a -> PA.ReaderOptions -> a -> ToPandoc m () -- add to current doc


-- | Pandoc output effect, take given doc and produce formatted output
data FromPandoc m r where
  WriteTo  :: PandocWriteFormat a -> PA.WriterOptions -> PA.Pandoc -> FromPandoc m a -- convert to given format


-- | add a piece of a Pandoc readable type to the current doc
addFrom
  :: P.Member ToPandoc effs
  => PandocReadFormat a
  -> PA.ReaderOptions
  -> a
  -> P.Semantic effs ()
addFrom prf pro doc = send $ AddFrom prf pro doc

-- | write given doc in requested format
writeTo
  :: P.Member FromPandoc effs
  => PandocWriteFormat a
  -> PA.WriterOptions
  -> PA.Pandoc
  -> P.Semantic effs a
writeTo pwf pwo pdoc = send $ WriteTo pwf pwo pdoc

-- | Convert a to Pandoc with the given options
toPandoc
  :: PA.PandocMonad m
  => PandocReadFormat a
  -> PA.ReaderOptions
  -> a
  -> m PA.Pandoc
toPandoc prf pro x = read pro x
 where
  read = case prf of
    ReadDocX       -> PA.readDocx
    ReadMarkDown   -> PA.readMarkdown
    ReadCommonMark -> PA.readCommonMark
    ReadRST        -> PA.readRST
    ReadLaTeX      -> PA.readLaTeX
    ReadHtml       -> PA.readHtml

-- | convert Pandoc to a with the given options
fromPandoc
  :: PA.PandocMonad m
  => PandocWriteFormat a
  -> PA.WriterOptions
  -> PA.Pandoc
  -> m a
fromPandoc pwf pwo pdoc = write pwo pdoc
 where
  write = case pwf of
    WriteDocX        -> PA.writeDocx
    WriteMarkDown    -> PA.writeMarkdown
    WriteCommonMark  -> PA.writeCommonMark
    WriteRST         -> PA.writeRST
    WriteLaTeX       -> PA.writeLaTeX
    WriteHtml5       -> PA.writeHtml5
    WriteHtml5String -> PA.writeHtml5String

-- | Re-interpret ToPandoc in Writer
toWriter
  :: PM.PandocEffects effs
  => P.Semantic (ToPandoc ': effs) a
  -> P.Semantic (P.Writer PA.Pandoc ': effs) a
toWriter =
  P.reinterpret (\(AddFrom rf ro x) -> P.raise (toPandoc rf ro x) >>= P.tell)

-- | run ToPandoc by interpreting in Writer and running that
runPandocWriter
  :: PM.PandocEffects effs
  => P.Semantic (ToPandoc ': effs) ()
  -> P.Semantic effs PA.Pandoc
runPandocWriter = fmap fst . P.runWriter . toWriter

-- | type-alias for use with the @Docs@ effect
type Pandocs = Docs PA.Pandoc

-- | add a new named Pandoc to a Pandoc Docs collection
newPandocPure
  :: P.Member Pandocs effs => T.Text -> PA.Pandoc -> P.Semantic effs ()
newPandocPure = newDoc

-- | add the Pandoc stored in the writer-style ToPandoc effect to the named docs collection with the given name
newPandoc
  :: (PM.PandocEffects effs, P.Member Pandocs effs)
  => T.Text
  -> P.Semantic (ToPandoc ': effs) ()
  -> P.Semantic effs ()
newPandoc n l = fmap fst (P.runWriter $ toWriter l) >>= newPandocPure n

-- | Given a write format and options, convert the NamedDoc to the requested format
namedPandocFrom
  :: PA.PandocMonad m
  => PandocWriteFormat a
  -> PA.WriterOptions
  -> NamedDoc PA.Pandoc
  -> m (NamedDoc a)
namedPandocFrom pwf pwo (NamedDoc n pdoc) = do
  doc <- fromPandoc pwf pwo pdoc
  return $ NamedDoc n doc

-- | Given a write format and options, convert a list of named Pandocs to a list of named docs in the requested format
pandocsToNamed
  :: PM.PandocEffects effs
  => PandocWriteFormat a
  -> PA.WriterOptions
  -> P.Semantic (Pandocs ': effs) ()
  -> P.Semantic effs [NamedDoc a]
pandocsToNamed pwf pwo =
  (traverse (namedPandocFrom pwf pwo) =<<) . toNamedDocList -- monad, list, NamedDoc itself

-- | Given a write format and options, run the writer-style ToPandoc effect and produce a doc of requested type
fromPandocE
  :: PM.PandocEffects effs
  => PandocWriteFormat a
  -> PA.WriterOptions
  -> P.Semantic (ToPandoc ': effs) ()
  -> P.Semantic effs a
fromPandocE pwf pwo = ((fromPandoc pwf pwo . fst) =<<) . P.runWriter . toWriter

