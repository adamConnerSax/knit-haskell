{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Control.Monad.Freer.Docs
  ( Docs
  , NamedDoc(..)
  , newDoc
  , toNamedDocList
  , mapNamedDocs
  , toNamedDocListWith
  , toNamedDocListWithM
  , mapNamedDocsM
  )
where

import qualified Data.Text                     as T
import qualified Control.Monad.Freer           as FR
import qualified Control.Monad.Freer.Writer    as FR

-- small effect for gathering up named documents into a list in order to handle output at one place

data Docs a r where
  NewDoc :: T.Text -> a -> Docs a ()

newDoc :: FR.Member (Docs a) effs => T.Text -> a -> FR.Eff effs ()
newDoc name doc = FR.send $ NewDoc name doc

-- interpret in State

data NamedDoc a = NamedDoc { ndName :: T.Text, ndDoc :: a } deriving (Functor, Foldable, Traversable)

toWriter
  :: FR.Eff (Docs a ': effs) () -> FR.Eff (FR.Writer [NamedDoc a] ': effs) ()
toWriter = FR.translate f
 where
  f :: Docs a x -> FR.Writer [NamedDoc a] x
  f r = case r of
    NewDoc n d -> FR.Tell [NamedDoc n d]

toNamedDocList :: FR.Eff (Docs a ': effs) () -> FR.Eff effs [NamedDoc a]
toNamedDocList = fmap snd . FR.runWriter . toWriter

--  FR.execState [] . toState

mapNamedDocs :: Monad m => (a -> b) -> m [NamedDoc a] -> m [NamedDoc b]
mapNamedDocs f = fmap (fmap (fmap f))

toNamedDocListWith
  :: (a -> b) -> FR.Eff (Docs a ': effs) () -> FR.Eff effs [NamedDoc b]
toNamedDocListWith f = mapNamedDocs f . toNamedDocList

mapNamedDocsM :: Monad m => (a -> m b) -> m [NamedDoc a] -> m [NamedDoc b]
mapNamedDocsM f = (traverse (traverse f) =<<) --join . fmap (traverse (traverse f))

toNamedDocListWithM
  :: (a -> FR.Eff effs b)
  -> FR.Eff (Docs a ': effs) ()
  -> FR.Eff effs [NamedDoc b]
toNamedDocListWithM f = mapNamedDocsM f . toNamedDocList

{-
toState :: FR.Eff ((Docs a) ': effs) () -> FR.Eff (FR.State [NamedDoc a]  ': effs) ()
toState = FR.reinterpret f where
  f :: FR.Member (FR.State [NamedDoc a]) effs => Docs a x -> FR.Eff effs x
  f r = case r of
    NewDoc n d -> FR.modify (\l -> (NamedDoc n d) : l)
-}
