{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds #-}
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
  , knitHtmlViaPandocIO
  , knitHtmlsViaPandocIO

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
-- This uses PandocIO to handle the effects Pandoc requires and then "runs" it, leaving an IO effect.
knitHtmlsViaPandocIO
  :: (LastMember (P.Lift PA.PandocIO) (KnitEffectStack PA.PandocIO))
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic (KnitEffectDocsStack PA.PandocIO) () -- ^ Knit effects "over" PandocIO
  -> IO [KP.NamedDoc TL.Text]
knitHtmlsViaPandocIO loggingPrefixM ls writeConfig x = do
  res <- PA.runIO $ knitHtmls loggingPrefixM ls writeConfig x
  case res of
    Left  err  -> (putStrLn $ "Pandoc Error: " ++ show err) >> return []
    Right docs -> return docs

-- | Create HTML Text from pandoc fragments
-- This uses PandocIO to handle the effects Pandoc requires and then "runs" it, leaving an IO effect.
knitHtmlViaPandocIO
  :: (LastMember (P.Lift PA.PandocIO) (KnitEffectStack PA.PandocIO))
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic (KnitEffectDocStack PA.PandocIO) () -- ^ Knit effects "over" PandocIO
  -> IO (Maybe TL.Text) -- ^  document, converted to Html as Text.
knitHtmlViaPandocIO loggingPrefixM ls writeConfig x = do
  res <- PA.runIO $ knitHtml loggingPrefixM ls writeConfig x
  case res of
    Left  err -> (putStrLn $ "Pandoc Error: " ++ show err) >> return Nothing
    Right doc -> return $ Just doc

-- | Create multiple HTML docs (as Text) from the named sets of pandoc fragments.
-- This allows use of any underlying monad to handle the Pandoc effects.  
knitHtmls
  :: forall m
   . ( PA.PandocMonad m
     , MonadIO m
     , MonadError PA.PandocError m
     , LastMember (P.Lift m) (KnitEffectStack m)
     )
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic (KnitEffectDocsStack m) () -- ^ Knit effects "over" m
  -> m [KP.NamedDoc TL.Text] -- ^  named documents, converted to Html as Text.
knitHtmls loggingPrefixM ls writeConfig x = do
  res :: Either PA.PandocError [KP.NamedDoc TL.Text] <- runSemT
    (consumeKnitEffectMany loggingPrefixM ls writeConfig)
    x
  case res of
    Left  err       -> throwError err
    Right namedDocs -> return namedDocs

-- | Create HTML Text from pandoc fragments
-- This allows use of any underlying monad to handle the Pandoc effects.  
knitHtml
  :: forall m
   . ( PA.PandocMonad m
     , MonadIO m
     , MonadError PA.PandocError m
     , LastMember (P.Lift m) (KnitEffectStack m)
     )
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Semantic (KnitEffectDocStack m) () -- ^ Knit effects "over" m
  -> m TL.Text -- ^  document, converted to Html as Text.
knitHtml loggingPrefixM ls writeConfig x = do
  res <- runSemT (consumeKnitEffectOne @m loggingPrefixM ls writeConfig) x
  case res of
    Left  err     -> throwError err
    Right docText -> return docText

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
   . (PA.PandocMonad m, MonadIO m, LastMember (P.Lift m) (KnitEffectStack m))
  => Maybe T.Text -- ^ outer logging prefix
  -> [KLog.LogSeverity] -- ^ what to output in log
  -> P.Semantic (KnitEffectStack m) a
  -> P.Semantic '[P.Lift m] (Either PA.PandocError a)
consumeKnitEffectStack loggingPrefixM ls =
  PI.runIO @m
    . PE.runError
    . KLog.filteredLogEntriesToIO ls
    . KPM.runPandoc @m -- PA.PandocIO
    . maybe id KLog.wrapPrefix loggingPrefixM

consumeKnitEffectMany
  :: forall m
   . ( PA.PandocMonad m
     , MonadIO m
     , MonadError PA.PandocError m
     , LastMember (P.Lift m) (KnitEffectStack m)
     )
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
   . ( PA.PandocMonad m
     , MonadIO m
     , MonadError PA.PandocError m
     , LastMember (P.Lift m) (KnitEffectStack m)
     )
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
