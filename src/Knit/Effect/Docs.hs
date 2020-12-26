{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.Docs
Description : Polysemy effect for creating a list of named documents
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

<https://github.com/isovector/polysemy#readme Polysemy> effect used by knit-haskell when one code-base is used to create multiple docs.
Each can be created and then stored in the
list maintained by this effect.  Then, when the effects are "run", this list can be processed to produce the required
output.
-}
module Knit.Effect.Docs
  (
    -- * Effect
    Docs

    -- * Actions
  , newDoc

    -- * Interpretations
  , toDocList
  , toDocListWith
  , toDocListWithM

    -- * Helper Types
  , DocWithInfo(..)

    -- * Helper Functions
  , mapDocs
  , mapDocsM
  )
where

import qualified Polysemy                      as P
import           Polysemy.Internal              ( send )
import qualified Polysemy.Writer               as P

-- | GADT to represent storing a document and some info for processing it.
data Docs i a m r where
  NewDoc ::i -> a -> Docs i a m ()

-- | Action of the 'Docs' Effect.  Store a document.
newDoc :: P.Member (Docs i a) effs => i -> a -> P.Sem effs ()
newDoc info doc = send $ NewDoc info doc

-- | Data type to hold one document with info of type @i@ and doc of type @a@. 
data DocWithInfo i a = DocWithInfo { dwiInfo :: i, dwiDoc :: a }
deriving instance Functor (DocWithInfo i)
deriving instance Foldable (DocWithInfo i)
deriving instance Traversable (DocWithInfo i)

-- | Intepret 'Docs' in @Polysemy.Writer [DocWithInfo i a]'
toWriter
  :: P.Sem (Docs i a ': effs) ()
  -> P.Sem (P.Writer [DocWithInfo i a] ': effs) ()
toWriter = P.reinterpret f
 where
  f :: Docs i a m x -> P.Sem (P.Writer [DocWithInfo i a] ': effs) x
  f (NewDoc i d) = P.tell [DocWithInfo i d]

-- | Interpret 'Docs' (via 'Polysemy.Writer'), producing a list of @DocWithInfo i a@
toDocList :: P.Sem (Docs i a ': effs) () -> P.Sem effs [DocWithInfo i a]
toDocList = fmap fst . P.runWriter . toWriter

-- | Map over the doc part of @Functor m => m [DocWithInfo i a]@ with an @a->b@ resulting in @m [DocWithInfo i b]@
mapDocs
  :: Monad m => (i -> a -> b) -> m [DocWithInfo i a] -> m [DocWithInfo i b]
mapDocs f = fmap (fmap (\(DocWithInfo i a) -> DocWithInfo i (f i a)))

-- | Map over the doc part of @Monad m => m [DocWithInfo i a]@ with @a -> m b@ resulting in @m [DocWithInfo i b]@
mapDocsM
  :: Monad m => (i -> a -> m b) -> m [DocWithInfo i a] -> m [DocWithInfo i b]
mapDocsM f = join . fmap (sequence . fmap (traverse id)) . mapDocs f --(traverse (traverse f) =<<)

-- | Combine the interpretation and mapping step.
-- Commonly used to "run" the effect and map the results to your desired output format.
toDocListWith
  :: (i -> a -> b)
  -> P.Sem (Docs i a ': effs) ()
  -> P.Sem effs [DocWithInfo i b]
toDocListWith f = mapDocs f . toDocList

-- | Combine the interpretation and effectful mapping step.
-- Commonly used to "run" the effect and map the results to your deisred output format.
toDocListWithM
  :: (i -> a -> P.Sem effs b)
  -> P.Sem (Docs i a ': effs) ()
  -> P.Sem effs [DocWithInfo i b]
toDocListWithM f = mapDocsM f . toDocList

