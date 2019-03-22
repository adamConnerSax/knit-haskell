{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Control.Monad.Freer.Pandoc
  ( ToPandoc
  , FromPandoc
  , PandocReadFormat(..)
  , PandocWriteFormat(..)
  , addFrom
  , runPandocWriter
  , writeTo
  , Pandocs
  , NamedDoc(..)
  , newPandoc
  , pandocsToNamed
  , fromPandocE
  , toPandoc
  , fromPandoc
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

data PandocReadFormat a where
  ReadDocX :: PandocReadFormat LBS.ByteString
  ReadMarkDown :: PandocReadFormat T.Text
  ReadCommonMark :: PandocReadFormat T.Text
  ReadRST :: PandocReadFormat T.Text
  ReadLaTeX :: PandocReadFormat T.Text
  ReadHtml :: PandocReadFormat T.Text

deriving instance Show (PandocReadFormat a)

data PandocWriteFormat a where
  WriteDocX :: PandocWriteFormat LBS.ByteString
  WriteMarkDown :: PandocWriteFormat T.Text
  WriteCommonMark :: PandocWriteFormat T.Text
  WriteRST :: PandocWriteFormat T.Text
  WriteLaTeX :: PandocWriteFormat T.Text
  WriteHtml5 :: PandocWriteFormat Blaze.Html -- Blaze
  WriteHtml5String :: PandocWriteFormat T.Text

deriving instance Show (PandocWriteFormat a)

data ToPandoc r where
  AddFrom  :: PandocReadFormat a -> P.ReaderOptions -> a -> ToPandoc () -- add to current doc

data FromPandoc r where
  WriteTo  :: PandocWriteFormat a -> P.WriterOptions -> P.Pandoc -> FromPandoc a -- convert to given format

addFrom
  :: FR.Member ToPandoc effs
  => PandocReadFormat a
  -> P.ReaderOptions
  -> a
  -> FR.Eff effs ()
addFrom prf pro doc = FR.send $ AddFrom prf pro doc

writeTo
  :: FR.Member FromPandoc effs
  => PandocWriteFormat a
  -> P.WriterOptions
  -> P.Pandoc
  -> FR.Eff effs a
writeTo pwf pwo pdoc = FR.send $ WriteTo pwf pwo pdoc

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

toWriter
  :: FR.PandocEffects effs
  => FR.Eff (ToPandoc ': effs) a
  -> FR.Eff (FR.Writer P.Pandoc ': effs) a
toWriter =
  FR.reinterpret (\(AddFrom rf ro x) -> FR.raise (toPandoc rf ro x) >>= FR.tell)

runPandocWriter
  :: FR.PandocEffects effs
  => FR.Eff (ToPandoc ': effs) ()
  -> FR.Eff effs P.Pandoc
runPandocWriter = fmap snd . FR.runWriter . toWriter

type Pandocs = Docs P.Pandoc

newPandocPure :: FR.Member Pandocs effs => T.Text -> P.Pandoc -> FR.Eff effs ()
newPandocPure = newDoc

newPandoc
  :: (FR.PandocEffects effs, FR.Member Pandocs effs)
  => T.Text
  -> FR.Eff (ToPandoc ': effs) ()
  -> FR.Eff effs ()
newPandoc n l = fmap snd (FR.runWriter $ toWriter l) >>= newPandocPure n

namedPandocFrom
  :: P.PandocMonad m
  => PandocWriteFormat a
  -> P.WriterOptions
  -> NamedDoc P.Pandoc
  -> m (NamedDoc a)
namedPandocFrom pwf pwo (NamedDoc n pdoc) = do
  doc <- fromPandoc pwf pwo pdoc
  return $ NamedDoc n doc

pandocsToNamed
  :: FR.PandocEffects effs
  => PandocWriteFormat a
  -> P.WriterOptions
  -> FR.Eff (Pandocs ': effs) ()
  -> FR.Eff effs [NamedDoc a]
pandocsToNamed pwf pwo =
  (traverse (namedPandocFrom pwf pwo) =<<) . toNamedDocList -- monad, list, NamedDoc itself

fromPandocE
  :: FR.PandocEffects effs
  => PandocWriteFormat a
  -> P.WriterOptions
  -> FR.Eff (ToPandoc ': effs) ()
  -> FR.Eff effs a
fromPandocE pwf pwo =
  ((fromPandoc pwf pwo . snd) =<<) . FR.runWriter . toWriter

