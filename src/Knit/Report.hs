{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ConstraintKinds      #-}
{-|
Module      : Knit.Report
Description : Re-export all the things required for pandoc-built reports.
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module re-exports the basic pieces to build reports using Pandoc
as well as providing some functions to do the "knitting"--produce the documents--for some common setups.

Notes:

1. The "Knit.Effect.RandomFu" effect is not imported since the names might clash with Polysemy.Random.
Import either effect directly if you need it.
2. Logger functions are imported but assume you will use the 'LogEntry' type.
3. The PandocMarkDown module is exported so if you want to use a different markdown flavor you may need to hide "addMarkDown" when you import this module.
-}
module Knit.Report
  (
    -- * Knit
    knitHtml
  , knitHtmls

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
  )
where

import           Polysemy                       ( Member
                                                , Semantic
                                                )
import           Knit.Effect.Pandoc             ( ToPandoc
                                                , Requirement(..)
                                                , PandocReadFormat(..)
                                                , PandocWriteFormat(..)
                                                , Pandocs
                                                )
import           Knit.Effect.PandocMonad        ( Pandoc
                                                , PandocEffects
                                                , runPandocAndLoggingToIO
                                                )
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


import           Control.Monad.Except           ( MonadError(..)
                                                , MonadIO
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import qualified Polysemy                      as P
import qualified Polysemy.Error                as PE
import qualified Polysemy.IO                   as PI

import qualified Text.Pandoc                   as PA
import qualified Text.Pandoc.Class             as PA
import qualified Text.Blaze.Html.Renderer.Text as BH


import qualified Knit.Report.Output.Html       as KO
import qualified Knit.Effect.Docs              as KD
import qualified Knit.Effect.Pandoc            as KP
import qualified Knit.Effect.PandocMonad       as KPM
import qualified Knit.Effect.Logger            as KLog

type KnitEffectsMany m =
  '[ KD.Docs KP.PandocWithRequirements
   , KPM.Pandoc
   , KLog.Logger KLog.LogEntry
   , KLog.PrefixLog
   , PE.Error PA.PandocError
   , P.Lift IO
   , P.Lift m
   ]

type KnitEffectsManyC m = (P.Member (P.Lift m) (KnitEffectsMany m))

knitHtmls
  :: forall m r
   . ( PA.PandocMonad m
     , MonadIO m
     , MonadError PA.PandocError m
     , KnitEffectsManyC m
     )
  => (forall a . P.Semantic r a -> P.Semantic (KnitEffectsMany m) a) -- ^ run any other effects.  Could be @id@.
  -> Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic r () -- ^ Knit effects "over" m
  -> m [KP.NamedDoc TL.Text] -- ^  named documents, converted to Html as Text.
knitHtmls runOthers loggingPrefixM ls writeConfig x = do
  res :: Either PA.PandocError [KP.NamedDoc TL.Text] <-
    P.runM
    . PI.runIO @m --PA.PandocIO
    . PE.runError
    . KLog.filteredLogEntriesToIO ls
    . KPM.runPandoc @m -- PA.PandocIO
    . maybe id KLog.wrapPrefix loggingPrefixM
    . KD.toNamedDocListWithM
        (fmap BH.renderHtml . KO.toBlazeDocument writeConfig)
    $ runOthers x
  case res of
    Left  err       -> throwError err
    Right namedDocs -> return namedDocs

type KnitEffectsOne m =
  '[ KP.ToPandoc
   , KPM.Pandoc
   , KLog.Logger KLog.LogEntry
   , KLog.PrefixLog
   , PE.Error PA.PandocError
   , P.Lift IO
   , P.Lift m
   ]

type KnitEffectsOneC m = (P.Member (P.Lift m) (KnitEffectsMany m))

knitHtml
  :: forall r
   . (forall a . P.Semantic r a -> P.Semantic (KnitEffectsOne PA.PandocIO) a) -- ^ run any other effects.  Could be @id@.
  -> Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic r () -- ^ Knit effects "over" m
  -> IO (Maybe TL.Text) -- ^  document, converted to Html as Text.
knitHtml runOthers loggingPrefixM ls writeConfig x = do
  res :: Either PA.PandocError TL.Text <-
    fmap (fmap BH.renderHtml . KPM.mergeEithers)
    . PA.runIO
    . P.runM
    . PI.runIO @PA.PandocIO
    . PE.runError
    . KLog.filteredLogEntriesToIO ls
    . KPM.runPandoc @PA.PandocIO
    . maybe id KLog.wrapPrefix loggingPrefixM
    . KO.pandocWriterToBlazeDocument writeConfig
    $ runOthers x
  case res of
    Left  err       -> (putStrLn $ "Pandoc Error" ++ show err) >> return Nothing
    Right namedDocs -> return $ Just namedDocs
