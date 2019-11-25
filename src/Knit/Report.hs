{-# LANGUAGE ExtendedDefaultRules #-}
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

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.

Notes:

1. You can add logging from within document creation using 'logLE'.
2. The "Knit.Report.Input.MarkDown.PandocMarkDown" module is exported
so if you want to use a different markdown flavor you may need to hide "addMarkDown" when you import this module.
3. If you use any other effects in your polysemy stack (e.g., Random or RandomFu), you will need to interpret/run them before calling knitHtml/knitHtmls.
-}
module Knit.Report
  (
    -- * Knit
    knitHtml
  , knitHtmls
  , liftKnit
  , knitError
  , knitMaybe
  , knitEither
  , knitMapError
  , KnitEffects
  , KnitOne
  , KnitMany
  , KnitBase

    -- * Inputs
  , module Knit.Report.Input.Table.Colonnade
  , module Knit.Report.Input.MarkDown.PandocMarkDown
  , module Knit.Report.Input.Html
  , module Knit.Report.Input.Html.Blaze
  , module Knit.Report.Input.Html.Lucid
  , module Knit.Report.Input.Latex
  , module Knit.Report.Input.Visualization.Hvega
  , module Knit.Report.Input.Visualization.Diagrams

    -- * Output
  , module Knit.Report.Output
  , module Knit.Report.Output.Html

    -- * Effects
  , module Polysemy  
  , module Knit.Effect.Pandoc
  , module Knit.Effect.Docs
  , module Knit.Effect.PandocMonad
  , module Knit.Effect.Logger
  , module Knit.Effect.UnusedId
  , module Polysemy.Async
  )
where

import           Polysemy                       ( Member
                                                , Sem
                                                , Embed
                                                , embed
                                                )
import           Polysemy.Async                 ( async
                                                , await
                                                , sequenceConcurrently)                 
import           Knit.Effect.Pandoc             ( ToPandoc
                                                , Requirement(..)
                                                , PandocReadFormat(..)
                                                , PandocWriteFormat(..)
                                                , Pandocs
                                                , PandocInfo(..)
                                                , newPandoc
                                                )
import           Knit.Effect.Docs               ( DocWithInfo(..) )
import           Knit.Effect.PandocMonad
import           Knit.Effect.Logger             ( LogSeverity(..)
                                                , logAll
                                                , nonDiagnostic
                                                , logLE
                                                , wrapPrefix
                                                , filteredLogEntriesToIO
                                                , LogWithPrefixesLE
                                                )
import           Knit.Report.Input.Table.Colonnade
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
import           Knit.Report.Input.Visualization.Diagrams
--                                         hiding ( trace ) -- trace conflicts with Pandoc.trace

import           Knit.Report.Output
import           Knit.Report.Output.Html        ( pandocWriterToBlazeDocument
                                                , mindocOptionsF
                                                , writeAllPandocResultsWithInfoAsHtml
                                                , writePandocResultWithInfoAsHtml
                                                )

import           Text.Pandoc                    ( PandocError(..) )

import           Control.Monad.Except           ( MonadIO )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import qualified Polysemy                      as P
import qualified Polysemy.Async                as P
import qualified Polysemy.Error                as PE
import qualified Polysemy.IO                   as PI

import qualified Text.Pandoc                   as PA
import qualified Text.Blaze.Html.Renderer.Text as BH

import qualified Knit.Report.Output.Html       as KO
import qualified Knit.Effect.Docs              as KD
import qualified Knit.Effect.Pandoc            as KP
import qualified Knit.Effect.PandocMonad       as KPM
import qualified Knit.Effect.Logger            as KLog
import qualified Knit.Effect.UnusedId          as KUI
import           Knit.Effect.UnusedId           ( getNextUnusedId )


-- | Create multiple HTML docs (as Text) from the named sets of pandoc fragments.
-- In use, you may need a type-application to specify m.
-- This allows use of any underlying monad to handle the Pandoc effects.
-- NB: Resulting documents are *Lazy* Text, as produced by the Blaze render function.
knitHtmls
  :: MonadIO m
  => Maybe T.Text -- ^ outer logging prefix
  -> (KLog.LogSeverity -> Bool) -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Sem (KnitEffectDocsStack m) ()
  -> m
       ( Either
           PA.PandocError
           [KP.DocWithInfo KP.PandocInfo TL.Text]
       )
knitHtmls loggingPrefixM logIf (PandocWriterConfig mFP tv oF) =
  consumeKnitEffectStack loggingPrefixM logIf . KD.toDocListWithM
    (\(KP.PandocInfo _ tv') a ->
      fmap BH.renderHtml
        . KO.toBlazeDocument (PandocWriterConfig mFP (tv' <> tv) oF)
        $ a
    )

-- | Create HTML Text from pandoc fragments
-- In use, you may need a type-application to specify m.
-- This allows use of any underlying monad to handle the Pandoc effects.
-- NB: Resulting document is *Lazy* Text, as produced by the Blaze render function.
knitHtml
  :: MonadIO m
  => Maybe T.Text -- ^ outer logging prefix
  -> (KLog.LogSeverity -> Bool) -- ^ what to output in log
  -> PandocWriterConfig -- ^ configuration for the Pandoc Html Writer
  -> P.Sem (KnitEffectDocStack m) ()
  -> m (Either PA.PandocError TL.Text)
knitHtml loggingPrefixM logIf writeConfig =
  fmap (fmap (fmap BH.renderHtml)) (consumeKnitEffectStack loggingPrefixM logIf)
    . KO.pandocWriterToBlazeDocument writeConfig

-- | Constraints required to knit a document using effects from a base monad m.
type KnitBase m effs = (MonadIO m, P.Member (P.Embed m) effs)

-- | lift an action in a base monad into a Polysemy monad.  This is just a renaming for convenience.
liftKnit :: Member (Embed m) r => m a -> Sem r a
liftKnit = P.embed

-- | Throw an error with a specific message.  This will emerge as a 'PandocSomeError' in order
-- to avoid complicating the error type.
-- NB: The Member constraint is satisfied by KnitEffectStack m.
knitError :: P.Member (PE.Error PA.PandocError) r => T.Text -> P.Sem r a
knitError msg =
  PE.throw (PA.PandocSomeError $ "Knit User Error: " ++ T.unpack msg)

-- | Throw on 'Nothing' with given message.  This will emerge as a 'PandocSomeError' in order
-- to avoid complicating the error type.
knitMaybe
  :: P.Member (PE.Error PA.PandocError) r => T.Text -> Maybe a -> P.Sem r a
knitMaybe msg = maybe (knitError msg) return

-- | Throw on 'Left' with message.  This will emerge as a 'PandocSomeError' in order
-- to avoid complicating the error type.
knitEither
  :: P.Member (PE.Error PA.PandocError) r => Either T.Text a -> P.Sem r a
knitEither = either knitError return




-- | Constraint alias for the effects we need (and run)
-- when calling Knit.
-- Anything inside a call to Knit can use any of these effects.
-- Any other effects will need to be run before @knitHtml(s)@
type KnitEffects r = (KPM.PandocEffects r
                     , P.Members [ KUI.UnusedId
                                 , KLog.Logger KLog.LogEntry
                                 , KLog.PrefixLog
                                 , P.Async
                                 , PE.Error PA.PandocError
                                 , P.Embed IO] r
                     )

-- | Constraint alias for the effects we need to knit one document
type KnitOne r = (KnitEffects r, P.Member KP.ToPandoc r)

-- | Constraint alias for the effects we need to knit multiple documents.
type KnitMany r = (KnitEffects r, P.Member KP.Pandocs r)


-- From here down is unexported.  
-- | The exact stack we are interpreting when we knit
type KnitEffectStack m
  = '[KUI.UnusedId, KPM.Pandoc, KLog.Logger KLog.LogEntry, KLog.PrefixLog, P.Async, PE.Error
    PA.PandocError, P.Embed IO, P.Embed m]
-- | Add a Multi-doc writer to the front of the effect list
type KnitEffectDocsStack m = (KP.Pandocs ': KnitEffectStack m)

-- | Add a single-doc writer to the front of the effect list
type KnitEffectDocStack m = (KP.ToPandoc ': KnitEffectStack m)

-- | Map an error type, @e, into a 'PandocError' so it will be handled in this stack
knitMapError
  :: forall e r a
   . KnitEffects r
  => (e -> T.Text)
  -> P.Sem (PE.Error e ': r) a
  -> P.Sem r a
knitMapError f = PE.mapError $ PandocSomeError . T.unpack . f

-- | run all knit-effects in @KnitEffectStack m@
consumeKnitEffectStack
  :: forall m a
   . MonadIO m
  => Maybe T.Text -- ^ outer logging prefix
  -> (KLog.LogSeverity -> Bool) -- ^ what to output in log
  -> P.Sem (KnitEffectStack m) a
  -> m (Either PA.PandocError a)
consumeKnitEffectStack loggingPrefixM logIf =
  P.runM
    . PI.embedToMonadIO @m -- interpret (Embed IO) using m
    . PE.runError
    . P.asyncToIO -- this has to run after (above) the log, partly so that the prefix state is thread-local.
    . KLog.filteredAsyncLogEntriesToIO logIf
    . KPM.interpretInIO -- PA.PandocIO
    . KUI.runUnusedId
    . maybe id KLog.wrapPrefix loggingPrefixM


