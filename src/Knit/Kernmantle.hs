{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
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
  , KnitCore
  , LogEff(..)
  , DocEff(..)
  , DocsEff(..)
  , KnitPipeline
  , DocPipeline
  , DocsPipeline
  , arrKnitCore
  , runInKnitCore
  , runDocPipeline
  , runDocPipeline'
  , runDocsPipeline
--  , newPandocA
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
type KnitCoreMantle r = [ '("log", LogEff), '("knitCore", KnitCore r)]

type KnitPipeline r a b
  = Rope.AnyRopeWith (KnitCoreMantle r) '[A.Arrow] a b

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


--type DocPipeline r a b
--  = Rope.AnyRopeWith  ('("doc", DocEff) ': KnitCoreMantle r) '[A.Arrow] a b

type DocPipeline r a b
  = Rope.TightRope  ('("doc", DocEff) ': KnitCoreMantle r) (KnitCore r) a b

runDocPipeline'
  :: K.KnitOne r
  => DocPipeline r a b
  -> a
  -> KnitMonad r b
runDocPipeline' pipeline input = pipeline
                                 & Rope.loosen -- so we can interpret them (?)
                                 & Rope.weaveK #doc runDocEffInKnitMonad
                                 & Rope.weaveK #log runLogEffInKnitMonad -- weaveK since this is interpreted in the core
                                 & Rope.weave' #knitCore id -- handle these directly in the core
                                 & Rope.untwine -- now the mantle is empty so get the core
                                 & (flip A.runKleisli input) -- run it with the input    


--type DocsPipeline r a b
--  = Rope.AnyRopeWith ('("docs", DocsEff) ': KnitCoreMantle r) '[A.Arrow] a b

type DocsPipeline r a b
  = Rope.TightRope ('("docs",DocsEff) ': KnitCoreMantle r) (KnitCore r) a b


runDocsPipeline'
  :: K.KnitMany r
  => DocsPipeline r a b
  -> a
  -> KnitMonad r b
runDocsPipeline' pipeline input = pipeline
                                 & Rope.loosen -- so we can interpret them (?)
                                 & Rope.weaveK #docs runDocsEffInKnitMonad
                                 & Rope.weaveK #log runLogEffInKnitMonad -- weaveK since this is interpreted in the core
                                 & Rope.weave' #knitCore id -- handle these directly in the core
                                 & Rope.untwine -- now the mantle is empty so get the core
                                 & (flip A.runKleisli input) -- run it with the input    

runDocPipeline :: MonadIO m
               => K.KnitConfig
               -> DocPipeline (K.KnitEffectDocStack m) a ()
               -> a
               -> m (Either PA.PandocError TL.Text)
runDocPipeline config pipeline input = K.knitHtml config $ runDocPipeline' pipeline input

runDocsPipeline :: MonadIO m
                => K.KnitConfig
                -> DocsPipeline (K.KnitEffectDocsStack m) a ()
                -> a
                -> m (Either PA.PandocError [KP.DocWithInfo KP.PandocInfo TL.Text])
runDocsPipeline config pipeline input = K.knitHtmls config $ runDocsPipeline' pipeline input

-- The Logging Effect
data LogEff a b where
  LogText :: LogEff (K.LogSeverity, T.Text) ()
  AddPrefix :: LogEff T.Text ()
  RemovePrefix :: LogEff () ()

runLogEffInKnitMonad :: P.Members K.PrefixedLogEffectsLE r => a `LogEff` b -> a -> KnitMonad r b
runLogEffInKnitMonad LogText = K.wrapPrefix "Pipeline" . uncurry K.logLE 
runLogEffInKnitMonad AddPrefix = K.addPrefix
runLogEffInKnitMonad RemovePrefix = const K.removePrefix

-- We add effects to write docs but then remove them when we run the pipeline

-- The Doc Writer Effect (add fragments to a pandoc)
data DocEff a b where
  AddFrom :: DocEff (KP.PandocReadFormat a, PA.ReaderOptions, a) ()
  Require :: DocEff KP.Requirement ()

runDocEffInKnitMonad :: P.Member KP.ToPandoc r => a `DocEff` b -> a -> KnitMonad r b
runDocEffInKnitMonad AddFrom = \(rf, ro, a) -> KP.addFrom rf ro a
runDocEffInKnitMonad Require = KP.require

-- The Docs Writer Effect (add entire document to a collection of pandocs)
data DocsEff a b where
  NewPandoc :: DocsEff (KP.PandocInfo, KP.PandocWithRequirements) ()

runDocsEffInKnitMonad :: P.Member KP.Pandocs r => a `DocsEff` b -> a -> KnitMonad r b
runDocsEffInKnitMonad NewPandoc = uncurry KP.newPandocPure

{-
newPandocA'
  :: forall r a b.K.KnitMany r
  => PE.PandocInfo
  -> DocPipeline (PE.ToPandoc ': r) a b
  -> DocsPipeline r a b
newPandocA' info docPipeline = proc a -> do
  
  Rope.strand #docs NewPandoc -< $ PE.runPandocWriter $ runDocPipeline' docPipeline a
-}
