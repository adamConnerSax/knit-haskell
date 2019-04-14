{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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
module Knit.Effects.Docs
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
import qualified Polysemy                      as P
import           Polysemy.Internal              ( send )
import qualified Polysemy.Writer               as P

-- small effect for gathering up named documents into a list in order to handle output at one place
-- | GADT to represent the effect.
data Docs a m r where
  NewDoc :: T.Text -> a -> Docs a m ()


-- | this function is the way users will use this effect
newDoc :: P.Member (Docs a) effs => T.Text -> a -> P.Semantic effs ()
newDoc name doc = send $ NewDoc name doc


-- | data type to hold one named document 
data NamedDoc a = NamedDoc { ndName :: T.Text, ndDoc :: a } deriving (Functor, Foldable, Traversable)

toWriter
  :: P.Semantic (Docs a ': effs) ()
  -> P.Semantic (P.Writer [NamedDoc a] ': effs) ()
toWriter = P.reinterpret f
 where
  f :: Docs a m x -> P.Semantic (P.Writer [NamedDoc a] ': effs) x
  f (NewDoc n d) = P.tell [NamedDoc n d]

-- | run this effect, producing a list of @NamedDoc 
toNamedDocList
  :: P.Typeable a
  => P.Semantic (Docs a ': effs) ()
  -> P.Semantic effs [NamedDoc a]
toNamedDocList = fmap fst . P.runWriter . toWriter

-- | map over the document part of an effecful doc list
mapNamedDocs :: Monad m => (a -> b) -> m [NamedDoc a] -> m [NamedDoc b]
mapNamedDocs f = fmap (fmap (fmap f))

-- | run the effect and map the docs to a new type.
toNamedDocListWith
  :: P.Typeable a
  => (a -> b)
  -> P.Semantic (Docs a ': effs) ()
  -> P.Semantic effs [NamedDoc b]
toNamedDocListWith f = mapNamedDocs f . toNamedDocList

-- | map over the document part of an effectful doc-list with an effectful function
mapNamedDocsM :: Monad m => (a -> m b) -> m [NamedDoc a] -> m [NamedDoc b]
mapNamedDocsM f = (traverse (traverse f) =<<) --join . fmap (traverse (traverse f))

-- | run the effect and map over the docs with an effectful function
toNamedDocListWithM
  :: P.Typeable a
  => (a -> P.Semantic effs b)
  -> P.Semantic (Docs a ': effs) ()
  -> P.Semantic effs [NamedDoc b]
toNamedDocListWithM f = mapNamedDocsM f . toNamedDocList

