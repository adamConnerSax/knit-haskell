{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
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
  , toNamedDocList
  , toNamedDocListWith
  , toNamedDocListWithM

    -- * Helper Types
  , NamedDoc(..)

    -- * Helper Functions
  , mapNamedDocs
  , mapNamedDocsM
  )
where

import qualified Data.Text                     as T
import qualified Polysemy                      as P
import           Polysemy.Internal              ( send )
import qualified Polysemy.Writer               as P


-- | GADT to represent storing a named document.
data Docs a m r where
  NewDoc :: T.Text -> a -> Docs a m ()

-- | Action of the 'Docs' Effect.  Store a named document.
newDoc :: P.Member (Docs a) effs => T.Text -> a -> P.Sem effs ()
newDoc name doc = send $ NewDoc name doc

-- | Data type to hold one named document of type @a@. 
data NamedDoc a = NamedDoc { ndName :: T.Text, ndDoc :: a } deriving (Functor, Foldable, Traversable)

-- | Intepret 'Docs' in @Polysemy.Writer [NamedDoc a]'
toWriter
  :: P.Sem (Docs a ': effs) () -> P.Sem (P.Writer [NamedDoc a] ': effs) ()
toWriter = P.reinterpret f
 where
  f :: Docs a m x -> P.Sem (P.Writer [NamedDoc a] ': effs) x
  f (NewDoc n d) = P.tell [NamedDoc n d]

-- | Interpret 'Docs' (via 'Polysemy.Writer'), producing a list of @NamedDoc a@
toNamedDocList :: P.Sem (Docs a ': effs) () -> P.Sem effs [NamedDoc a]
toNamedDocList = fmap fst . P.runWriter . toWriter

-- | Map over the doc part of @Functor m => m [NamedDoc a]@ with an @a->b@ resulting in @m [NamedDoc b]@
mapNamedDocs :: Monad m => (a -> b) -> m [NamedDoc a] -> m [NamedDoc b]
mapNamedDocs f = fmap (fmap (fmap f))

-- | Map over the doc part of @Monad m => m [NamedDoc a]@ with @a -> m b@ resulting in @m [NamedDoc b]@
mapNamedDocsM :: Monad m => (a -> m b) -> m [NamedDoc a] -> m [NamedDoc b]
mapNamedDocsM f = (traverse (traverse f) =<<)

-- | Combine the interpretation and mapping step.  Commonly used to "run" the effect and map the results to your deisred output format.
toNamedDocListWith
  :: (a -> b) -> P.Sem (Docs a ': effs) () -> P.Sem effs [NamedDoc b]
toNamedDocListWith f = mapNamedDocs f . toNamedDocList

-- | Combine the interpretation and effectful mapping step.  Commonly used to "run" the effect and map the results to your deisred output format.
toNamedDocListWithM
  :: (a -> P.Sem effs b) -> P.Sem (Docs a ': effs) () -> P.Sem effs [NamedDoc b]
toNamedDocListWithM f = mapNamedDocsM f . toNamedDocList

