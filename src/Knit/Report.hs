{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Knit.Report
Description : Re-export all the things required for pandoc-built reports.
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module re-exports the basic pieces to build reports using Pandoc
as well as providing functions to do the "knitting"--produce the documents.
That is, it is intended as one-stop-shopping for using this library to produce Html from various fragments which
Pandoc can read.

Notes:

1. The "Knit.Effect.RandomFu" effect is not imported since the names might clash with Polysemy.Random.
Import either effect directly if you need it.
2. Logger functions are imported but assume you will use the 'LogEntry' type.
3. The PandocMarkDown module is exported so if you want to use a different markdown flavor you may need to hide "addMarkDown" when you import this module.
4. If you use any other effects in your polysemy stack (e.g., Random or RandomFu), you will need to interpret/run them before calling knitHtml/knitHtmls.
-}
module Knit.Report
  (
    -- * Knit
    knitHtml
  , knitHtmls
  , liftKnit
  , KnitBase

    -- * Inputs
  , module Knit.Report.Input.MarkDown.PandocMarkDown
  , module Knit.Report.Input.Html
  , module Knit.Report.Input.Html.Blaze
  , module Knit.Report.Input.Html.Lucid
  , module Knit.Report.Input.Latex
  , module Knit.Report.Input.Visualization.Hvega

    -- * Output Formats
  , module Knit.Report.Output
  , module Knit.Report.Output.Html

    -- * Effects
  , module Polysemy
  , module Knit.Effect.Pandoc
  , module Knit.Effect.PandocMonad
  , module Knit.Effect.Logger

    -- * Pandoc
  , PandocMonad
  , PandocIO

    -- * IO    
  , MonadIO

    -- * Error
  , MonadError
  )
where

import           Polysemy                       ( Member
                                                , Semantic
                                                , Lift
                                                )
import           Knit.Effect.Pandoc             ( ToPandoc
                                                , Requirement(..)
                                                , PandocReadFormat(..)
                                                , PandocWriteFormat(..)
                                                , Pandocs
                                                )
import           Knit.Effect.PandocMonad
import           Knit.Effect.Logger             ( LogSeverity(..)
                                                , logAll
                                                , nonDiagnostic
                                                , logLE
                                                , wrapPrefix
                                                , filteredLogEntriesToIO
                                                , LogWithPrefixesLE
                                                )
import           Knit.Report.Input.MarkDown.PandocMarkDown
                                                ( addMarkDown )
import           Knit.Report.Input.Html         ( addStrictTextHtml
                                                , addLazyTextHtml
                                                )
import           Knit.Report.Input.Html.Blaze   ( addBlaze )
import           Knit.Report.Input.Html.Lucid   ( addLucid )
import           Knit.Report.Input.Latex        ( addLatex )
import           Knit.Report.Input.Visualization.Hvega
                                                ( addHvega )

import           Knit.Report.Output             ( PandocWriterConfig(..) )
import           Knit.Report.Output.Html        ( pandocWriterToBlazeDocument
                                                , mindocOptionsF
                                                )

import           Text.Pandoc                    ( PandocError )
import           Text.Pandoc.Class              ( PandocMonad
                                                , PandocIO
                                                )


import           Control.Monad.Except           ( MonadError(..)
                                                , MonadIO
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           GHC.Exts                       ( Constraint )

import qualified Polysemy                      as P
import qualified Polysemy.Error                as PE
import qualified Polysemy.IO                   as PI

import qualified Text.Pandoc                   as PA
--import qualified Text.Pandoc.Class             as PA
import qualified Text.Blaze.Html.Renderer.Text as BH


import qualified Knit.Report.Output.Html       as KO
import qualified Knit.Effect.Docs              as KD
import qualified Knit.Effect.Pandoc            as KP
import qualified Knit.Effect.PandocMonad       as KPM
import qualified Knit.Effect.Logger            as KLog



-- | Create multiple HTML docs (as Text) from the named sets of pandoc fragments.
-- In use, you may need a type-application to specify m.
-- This allows use of any underlying monad to handle the Pandoc effects.  
knitHtmls
  :: forall m
   . (MonadIO m
--     , MonadError PA.PandocError m
               , LastMember (P.Lift m) (KnitEffectStack m))
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic (KnitEffectDocsStack m) () -- ^ Knit effects "over" m
  -> m (Either PA.PandocError [KP.NamedDoc TL.Text]) -- ^  named documents, converted to Html as Text or error
knitHtmls loggingPrefixM ls writeConfig =
  runSemT (consumeKnitEffectMany loggingPrefixM ls writeConfig)

-- | Create HTML Text from pandoc fragments
-- In use, you may need a type-application to specify m.
-- This allows use of any underlying monad to handle the Pandoc effects.  
knitHtml
  :: forall m
   . (MonadIO m, LastMember (P.Lift m) (KnitEffectStack m))
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic (KnitEffectDocStack m) () -- ^ Knit effects "over" m
  -> m (Either PA.PandocError TL.Text) -- ^  document, converted to Html as Text, or error
knitHtml loggingPrefixM ls writeConfig x =
  runSemT (consumeKnitEffectOne loggingPrefixM ls writeConfig) x

type KnitBase m effs = (P.Member (P.Lift m) effs)

liftKnit :: Member (Lift m) r => m a -> Semantic r a
liftKnit = P.sendM

-- From here down is unexported.  

type KnitEffectStack m =
  '[ KPM.Pandoc
   , KLog.Logger KLog.LogEntry
   , KLog.PrefixLog
   , PE.Error PA.PandocError
   , P.Lift IO
   , P.Lift m
   ]

type KnitEffectDocsStack m = (KD.Docs KP.PandocWithRequirements ': KnitEffectStack m)
type KnitEffectDocStack m = (KP.ToPandoc ': KnitEffectStack m)

type family LastMember (eff :: k) (r :: [k]) :: Constraint where
  LastMember eff '[] = ()
  LastMember eff (e : es) = (P.Member eff (e ': es), LastMember eff es)

runSemT
  :: Monad m
  => (P.Semantic r a -> P.Semantic '[P.Lift m] b)
  -> P.Semantic r a
  -> m b
runSemT consume = P.runM . consume

consumeKnitEffectStack
  :: forall m a
   . (MonadIO m, LastMember (P.Lift m) (KnitEffectStack m))
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> P.Semantic (KnitEffectStack m) a
  -> P.Semantic '[P.Lift m] (Either PA.PandocError a)
consumeKnitEffectStack loggingPrefixM ls =
  PI.runIO @m -- interpret (Lift IO) using m
    . PE.runError
    . KLog.filteredLogEntriesToIO ls
    . KPM.interpretInIO -- PA.PandocIO
    . maybe id KLog.wrapPrefix loggingPrefixM

consumeKnitEffectMany
  :: forall m
   . (MonadIO m, LastMember (P.Lift m) (KnitEffectStack m))
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic (KnitEffectDocsStack m) ()
  -> P.Semantic
       '[P.Lift m]
       (Either PA.PandocError [KP.NamedDoc TL.Text])
consumeKnitEffectMany loggingPrefixM ls writeConfig =
  consumeKnitEffectStack @m loggingPrefixM ls . KD.toNamedDocListWithM
    (fmap BH.renderHtml . KO.toBlazeDocument writeConfig)

consumeKnitEffectOne
  :: forall m
   . (MonadIO m, LastMember (P.Lift m) (KnitEffectStack m))
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic (KnitEffectDocStack m) ()
  -> P.Semantic '[P.Lift m] (Either PA.PandocError TL.Text)
consumeKnitEffectOne loggingPrefixM ls writeConfig =
  fmap (fmap (fmap BH.renderHtml)) (consumeKnitEffectStack @m loggingPrefixM ls)
    . KO.pandocWriterToBlazeDocument writeConfig


{-
type KnitEffectC m r = (
    P.Member KPM.Pandoc r
  , KLog.LogWithPrefixesLE r
  , P.Member (PE.Error PA.PandocError) r
  , P.Member (P.Lift IO) r
  , P.Member (P.Lift m) r
  )

consumeKnitEffectPoly
  :: forall m r a
   . (PA.PandocMonad m, MonadIO m, KnitEffectC m r, LastMember (P.Lift m) r)
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> P.Semantic r a
  -> P.Semantic '[P.Lift m] (Either PA.PandocError a)
consumeKnitEffectPoly loggingPrefixM ls =
  PI.runIO @m
    . PE.runError
    . KLog.filteredLogEntriesToIO ls
    . KPM.runPandoc @m -- PA.PandocIO
    . maybe id KLog.wrapPrefix loggingPrefixM
-}
