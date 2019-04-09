{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Control.Monad.Freer.Docs
Description : freer-simple effect for creating a list of named documents
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Used by knit-haskell when one code-base is used to create multiple docs.  Each can be created and then stored in the
list maintained by this effect.  Then, when the effects are "run", this list can be processed to produce the required
output.
-}
module Control.Monad.Freer.Docs
  (
    -- * Types
    Docs
  , NamedDoc(..)
  -- * Store a document
  , newDoc
  -- * run the effect
  , toNamedDocList
  , toNamedDocListWith
  , toNamedDocListWithM
  -- * map over doc-lists
  , mapNamedDocs
  , mapNamedDocsM
  )
where

import qualified Data.Text                     as T
import qualified Control.Monad.Freer           as FR
import qualified Control.Monad.Freer.Writer    as FR

-- small effect for gathering up named documents into a list in order to handle output at one place
-- | GADT to represent the effect.
data Docs a r where
  NewDoc :: T.Text -> a -> Docs a ()

-- | this function is the way users will use this effect
newDoc :: FR.Member (Docs a) effs => T.Text -> a -> FR.Eff effs ()
newDoc name doc = FR.send $ NewDoc name doc

-- | data type to hold one named document 
data NamedDoc a = NamedDoc { ndName :: T.Text, ndDoc :: a } deriving (Functor, Foldable, Traversable)

toWriter
  :: FR.Eff (Docs a ': effs) () -> FR.Eff (FR.Writer [NamedDoc a] ': effs) ()
toWriter = FR.translate f
 where
  f :: Docs a x -> FR.Writer [NamedDoc a] x
  f r = case r of
    NewDoc n d -> FR.Tell [NamedDoc n d]

-- | run this effect, producing a list of @NamedDoc 
toNamedDocList :: FR.Eff (Docs a ': effs) () -> FR.Eff effs [NamedDoc a]
toNamedDocList = fmap snd . FR.runWriter . toWriter

-- | map over the document part of an effecful doc list
mapNamedDocs :: Monad m => (a -> b) -> m [NamedDoc a] -> m [NamedDoc b]
mapNamedDocs f = fmap (fmap (fmap f))

-- | run the effect and map the docs to a new type.
toNamedDocListWith
  :: (a -> b) -> FR.Eff (Docs a ': effs) () -> FR.Eff effs [NamedDoc b]
toNamedDocListWith f = mapNamedDocs f . toNamedDocList

-- | map over the document part of an effectful doc-list with an effectful function
mapNamedDocsM :: Monad m => (a -> m b) -> m [NamedDoc a] -> m [NamedDoc b]
mapNamedDocsM f = (traverse (traverse f) =<<) --join . fmap (traverse (traverse f))

-- | run the effect and map over the docs with an effectful function
toNamedDocListWithM
  :: (a -> FR.Eff effs b)
  -> FR.Eff (Docs a ': effs) ()
  -> FR.Eff effs [NamedDoc b]
toNamedDocListWithM f = mapNamedDocsM f . toNamedDocList

