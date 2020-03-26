{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.Pandoc
Description : Polysemy writer-like effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Polysemy Pandoc effect.
This is writer-like, allowing the interspersed addition of various Pandoc-readable formats into one doc and then rendering
to many Pandoc-writeable formats.  Currently only a subset of formats are supported.  Inputs can express requirements,
e.g., hvega requires html output because it uses javascript.
Those requirements are then checked before output is rendered and an error thrown if the input is not supported.
-}
module Knit.Effect.Pandoc
  (
    -- * Effects
    ToPandoc
  , FromPandoc

    -- * Requirement Support
  , Requirement(..)
  , PandocWithRequirements

    -- * Format ADTs
  , PandocReadFormat(..)
  , PandocWriteFormat(..)

    -- * Combinators
  , addFrom
  , require
  , writeTo
  , toPandoc
  , fromPandoc

    -- * Interpreters
  , runPandocWriter

    -- * Docs effect type-aliases
  , Pandocs

    -- * Pandoc Specific Info
  , PandocInfo(..)

    -- * Docs Effect Interpreters 
  , newPandoc
  , newPandocPure
  , pandocsToDocs
  , fromPandocE

    -- * Re-exports
  , DocWithInfo(..)
  )
where

import qualified Text.Pandoc                   as PA
import qualified Data.Text                     as T
import           Data.ByteString.Lazy          as LBS
import qualified Data.Foldable                 as F
import qualified Data.Map                      as M
import qualified Data.Monoid                   as Mon
import           Data.Set                      as S
import qualified Text.Blaze.Html               as Blaze
import           Control.Monad.Except           ( throwError )



import qualified Polysemy                      as P
import           Polysemy.Internal              ( send )
import qualified Polysemy.Writer               as P


import qualified Knit.Effect.PandocMonad       as PM

import           Knit.Effect.Docs               ( Docs
                                                , DocWithInfo(..)
                                                , newDoc
                                                , toDocList
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

-- | ADT to allow inputs to request support, if necessary or possible, in the output format.
-- E.g., Latex output in Html needs MathJax. But Latex needs to nothing to output in Latex.
-- Vega-lite needs some script headers to output in Html and can't be output in other formats.
-- For now, we support all the things we can in any output format so this just results
-- in a runtime test.

-- TODO (?): Allow headers/extensions to be added/switched based on this.
data Requirement
  =
    VegaSupport -- ^ Supported only for Html output.
  | LatexSupport -- ^ Supported in Html output (via MathJax) and Latex output.
  deriving (Show, Ord, Eq, Bounded, Enum)

handlesAll :: PandocWriteFormat a -> S.Set Requirement -> Bool
handlesAll f rs = Mon.getAll
  $ F.fold (fmap (Mon.All . handles f) $ S.toList rs)
 where
  handles :: PandocWriteFormat a -> Requirement -> Bool
  handles WriteHtml5       VegaSupport  = True
  handles WriteHtml5String VegaSupport  = True
  handles WriteHtml5       LatexSupport = True
  handles WriteHtml5String LatexSupport = True
  handles WriteLaTeX       LatexSupport = True
  handles _                _            = False

data PandocWithRequirements = PandocWithRequirements { doc :: PA.Pandoc, reqs :: S.Set Requirement }
instance Semigroup PandocWithRequirements where
  (PandocWithRequirements da ra) <> (PandocWithRequirements db rb)
    = PandocWithRequirements (da <> db) (ra <> rb)

instance Monoid PandocWithRequirements where
  mempty = PandocWithRequirements mempty mempty


justDoc :: PA.Pandoc -> PandocWithRequirements
justDoc d = PandocWithRequirements d mempty

justRequirement :: Requirement -> PandocWithRequirements
justRequirement r = PandocWithRequirements mempty (S.singleton r)

-- | Pandoc writer, add any read format to current doc
data ToPandoc m r where
  AddFrom  :: PandocReadFormat a -> PA.ReaderOptions -> a -> ToPandoc m () -- ^ add to current doc
  Require :: Requirement -> ToPandoc m () -- ^ require specific support

-- | Pandoc output effect, take given doc and produce formatted output
data FromPandoc m r where
  WriteTo  :: PandocWriteFormat a -> PA.WriterOptions -> PA.Pandoc -> FromPandoc m a -- convert to given format

-- | Add a piece of a Pandoc readable type to the current doc
addFrom
  :: P.Member ToPandoc effs
  => PandocReadFormat a
  -> PA.ReaderOptions
  -> a
  -> P.Sem effs ()
addFrom prf pro doc' = send $ AddFrom prf pro doc'

-- | Add a requirement that the output format must satisfy.
require :: P.Member ToPandoc effs => Requirement -> P.Sem effs ()
require r = send $ Require r

-- | Write given doc in requested format
writeTo
  :: P.Member FromPandoc effs
  => PandocWriteFormat a
  -> PA.WriterOptions
  -> PA.Pandoc
  -> P.Sem effs a
writeTo pwf pwo pdoc = send $ WriteTo pwf pwo pdoc

-- | Convert a to Pandoc with the given options
toPandoc
  :: PA.PandocMonad m
  => PandocReadFormat a
  -> PA.ReaderOptions
  -> a
  -> m PA.Pandoc
toPandoc prf pro x = readF pro x
 where
  readF = case prf of
    ReadDocX       -> PA.readDocx
    ReadMarkDown   -> PA.readMarkdown
    ReadCommonMark -> PA.readCommonMark
    ReadRST        -> PA.readRST
    ReadLaTeX      -> PA.readLaTeX
    ReadHtml       -> PA.readHtml

-- | Convert Pandoc to requested format with the given options.
-- | Throw a PandocError if the output format is unsupported given the inputs.
fromPandoc
  :: PA.PandocMonad m
  => PandocWriteFormat a
  -> PA.WriterOptions
  -> PandocWithRequirements
  -> m a
fromPandoc pwf pwo (PandocWithRequirements pdoc rs) = case handlesAll pwf rs of
  False ->
    throwError
      $  PA.PandocSomeError
      $  PM.textToPandocText
      $  "One of "
      <> (T.pack $ show $ S.toList rs)
      <> " cannot be output to "
      <> (T.pack $ show pwf)
  True -> write pwo pdoc
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
  => P.Sem (ToPandoc ': effs) a
  -> P.Sem (P.Writer PandocWithRequirements ': effs) a
toWriter = P.reinterpret $ \case
  (AddFrom rf ro x) ->
    P.raise (fmap justDoc $ PM.absorbPandocMonad $ toPandoc rf ro x)
      >>= P.tell @PandocWithRequirements
  (Require r) -> P.tell (justRequirement r)

-- | Run ToPandoc by interpreting in Writer and then running that Writer.
runPandocWriter
  :: PM.PandocEffects effs
  => P.Sem (ToPandoc ': effs) ()
  -> P.Sem effs PandocWithRequirements
runPandocWriter = fmap fst . P.runWriter . toWriter

-- | Type to hold info about each document that will be required for rendering and output
data PandocInfo = PandocInfo { pdiName :: T.Text, pdiTemplateVars :: M.Map String String }

-- | Type-alias for use with the @Docs@ effect.
type Pandocs = Docs PandocInfo PandocWithRequirements


-- | Add a new named Pandoc to a Pandoc Docs collection.
newPandocPure
  :: P.Member Pandocs effs
  => PandocInfo  -- ^ name and template variables for document
  -> PandocWithRequirements -- ^ document and union of all input requirements
  -> P.Sem effs ()
newPandocPure = newDoc

-- | Add the Pandoc stored in the writer-style ToPandoc effect to the named docs collection with the given name.
newPandoc
  :: (PM.PandocEffects effs, P.Member Pandocs effs)
  => PandocInfo  -- ^ name and template variables for document
  -> P.Sem (ToPandoc ': effs) a
  -> P.Sem effs a
newPandoc n l = do
  (pdwr, a) <- P.runWriter $ toWriter l
  newPandocPure n pdwr
  return a

-- | Given a write format and options, convert the NamedDoc to the requested format
pandocFrom
  :: PA.PandocMonad m
  => PandocWriteFormat a -- ^ format for Pandoc output
  -> PA.WriterOptions -- ^ options for the Pandoc Writer
  -> DocWithInfo PandocInfo PandocWithRequirements -- ^ named Pandoc with its union of requirements
  -> m (DocWithInfo PandocInfo a) -- ^ document in output format (in the effects monad).
pandocFrom pwf pwo (DocWithInfo i pdoc) = do
  doc' <- fromPandoc pwf pwo pdoc
  return $ DocWithInfo i doc'

-- | Given a write format and options,
-- convert a list of named Pandocs to a list of named docs in the requested format
pandocsToDocs
  :: PM.PandocEffects effs
  => PandocWriteFormat a -- ^ format for Pandoc output
  -> PA.WriterOptions -- ^ options for the Pandoc Writer
  -> P.Sem (Pandocs ': effs) () -- ^ effects stack to be (partially) run to get documents
  -> P.Sem effs [DocWithInfo PandocInfo a] -- ^ documents in requested format, within the effects monad
pandocsToDocs pwf pwo =
  (traverse (\x -> PM.absorbPandocMonad $ pandocFrom pwf pwo x) =<<) . toDocList

-- | Given a write format and options, run the writer-style ToPandoc effect and produce a doc of requested type
fromPandocE
  :: PM.PandocEffects effs
  => PandocWriteFormat a -- ^ format for Pandoc output
  -> PA.WriterOptions -- ^ options for the Pandoc Writer
  -> P.Sem (ToPandoc ': effs) () -- ^ effects stack to be (partially) run to get document
  -> P.Sem effs a -- ^ document in requested format, within the effects monad
fromPandocE pwf pwo =
  (((\x -> PM.absorbPandocMonad $ fromPandoc pwf pwo x) . fst) =<<)
    . P.runWriter
    . toWriter

