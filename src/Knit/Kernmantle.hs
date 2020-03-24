{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Knit.Kernmantle
  (
    KnitKleisli
  , LogEff(..)
  , KnitPipeline
  , arrKnitCore
  , runInKnitCore
  , runDocPipeline
  ) where

import qualified Control.Kernmantle.Rope as Rope
import           Control.Kernmantle.Rope ((&))
import qualified Polysemy as P
import qualified Knit.Report.EffectStack as K
import qualified Knit.Effect.Logger as K
import qualified Knit.Effect.Pandoc as KP
import qualified Control.Arrow as A

import           Control.Monad.Except           ( MonadIO )
import qualified Data.Text as T
import qualified Data.Text.Lazy                as TL
import qualified Text.Pandoc                   as PA

-- Wrap the knit-haskell stack as a Kleisli arrow
-- and then we can dispatch effects to it.
-- We wil slowly build
-- these effects directly in Kernmantle as well,
-- eventually leaving the base as just the PandocMonad
-- and IO bits, hopefully.
-- Basically, leave the truly monadic bits in the core
-- and move anything fundamentally applicative to
-- the arrow/kernmantle layer.
type KnitMonad knitEffs = P.Sem knitEffs
type KnitKleisli knitEffs = A.Kleisli (KnitMonad knitEffs) 

-- this will evolve as we reinterpret and manage effects above the monadic Knit core
type KnitCore knitEffs = KnitKleisli knitEffs


-- run
-- To use proc notation on a pipeline of this type, we need to know that the core is an instance of Arrow.  So
-- we add that here.
type KnitPipeline knitEffs a b = Rope.AnyRopeWith '[ '("log", LogEff), '("knitCore", KnitCore knitEffs)] '[A.Arrow] a b

arrKnitCore :: (a -> KnitMonad knitEffs b) -> KnitPipeline knitEffs a b
arrKnitCore = Rope.strand #knitCore . A.Kleisli  

runInKnitCore ::  KnitMonad knitEffs b -> KnitPipeline knitEffs () b
runInKnitCore = arrKnitCore . const 

runKnitPipeline' :: K.KnitEffects knitEffs => KnitPipeline knitEffs a b -> a -> KnitMonad knitEffs b
runKnitPipeline' pipeline input = pipeline
                                 & Rope.loosen -- so we can interpret them (?)
                                 & Rope.weaveK #log runLogEffInKnitMonad -- weaveK since this is interpreted in the core
                                 & Rope.weave' #knitCore id -- handle these directly in the core
                                 & Rope.untwine -- now the mantle is empty so get the core
                                 & (flip A.runKleisli input) -- run it with the input                                

runDocPipeline :: MonadIO m => K.KnitConfig -> KnitPipeline (K.KnitEffectDocStack m) a () -> a -> m (Either PA.PandocError TL.Text)
runDocPipeline config pipeline input = K.knitHtml config $ runKnitPipeline' pipeline input

runDocsPipeline :: MonadIO m
                => K.KnitConfig
                -> KnitPipeline (K.KnitEffectDocsStack m) a ()
                -> a
                -> m (Either PA.PandocError [KP.DocWithInfo KP.PandocInfo TL.Text])
runDocsPipeline config pipeline input = K.knitHtmls config $ runKnitPipeline' pipeline input

-- The Logging Effect
data LogEff a b where
  LogText :: LogEff (K.LogSeverity, T.Text) ()

runLogEffInKnitMonad :: P.Members K.PrefixedLogEffectsLE knitEffs => a `LogEff` b -> a -> KnitMonad knitEffs b
runLogEffInKnitMonad LogText = K.wrapPrefix "Pipeline" . uncurry K.logLE 




--interpLogEff :: a `LogEff` b -> Rope.AnyRopeWith '[ '("knitCore", KnitCore m)] '[] a b 
--interpLogEff  = Rope.strand #knitCore . A.Kleisli . runLogEffInKnitMonad

