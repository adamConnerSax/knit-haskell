{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Knit.Report.EffectStack
Description : Knit effects stack, interpreters and configuration for Html reports
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module contains the core effect stack, interpreter and configurations for building Html reports.

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.
-}
module Knit.Report.EffectStack
  (
    -- * Configuraiton
    KnitConfig(..)
  , defaultKnitConfig
    -- * Knit
  , knitHtml
  , knitHtmls
  , liftKnit
  -- * Constraints for knit-haskell actions (see examples)
  , KnitEffects
  , KnitEffectStack
  , KnitOne
  , KnitMany
  , KnitBase
  )
where

import           Control.Monad.Except           ( MonadIO )
--import qualified Control.Monad.Catch as Exceptions (SomeException) 
--import qualified Data.ByteString               as BS
--import qualified Data.ByteString.Lazy          as BL
import           Data.Functor.Identity          (Identity)
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Word                     as Word 
import qualified Polysemy                      as P
import qualified Polysemy.Async                as P
import qualified Polysemy.Error                as PE
import qualified Polysemy.IO                   as PI
--import qualified Polysemy.ConstraintAbsorber.MonadCatch as Polysemy 

import qualified Streamly                      as Streamly
--import qualified Streamly.Memory.Array         as Streamly.Array

import qualified System.IO.Error               as IE

import qualified Text.Pandoc                   as PA
import qualified Text.Blaze.Html.Renderer.Text as BH



import qualified Knit.Report.Output            as KO
import qualified Knit.Report.Output.Html       as KO
import qualified Knit.Effect.Docs              as KD
import qualified Knit.Effect.Pandoc            as KP
import qualified Knit.Effect.PandocMonad       as KPM
import qualified Knit.Effect.Logger            as KLog
import qualified Knit.Effect.UnusedId          as KUI
import qualified Knit.Effect.AtomicCache       as KC

-- | Parameters for knitting. If possible, use this via, e.g., 
-- @
-- myConfig = defaultKnitConfig { cacheDir = "myCacheDir", pandocWriterConfig = myConfig }
-- @
-- so that your code will still compile if parameters are added to this structure.
data KnitConfig = KnitConfig { outerLogPrefix :: Maybe T.Text
                             , logIf :: KLog.LogSeverity -> Bool
                             , cacheDir :: T.Text
                             , pandocWriterConfig :: KO.PandocWriterConfig
                             }

-- | Sensible defaults for a knit configuration.
defaultKnitConfig :: KnitConfig
defaultKnitConfig = KnitConfig (Just "knit-haskell")
                               KLog.nonDiagnostic
                               ".knit-haskell-cache"
                               (KO.PandocWriterConfig Nothing M.empty id)

-- | Create multiple HTML docs (as Text) from the named sets of pandoc fragments.
-- In use, you may need a type-application to specify m.
-- This allows use of any underlying monad to handle the Pandoc effects.
-- NB: Resulting documents are *Lazy* Text, as produced by the Blaze render function.
knitHtmls
  :: MonadIO m
  => KnitConfig
  -> P.Sem (KnitEffectDocsStack m) ()
  -> m (Either PA.PandocError [KP.DocWithInfo KP.PandocInfo TL.Text])
knitHtmls config =
  let KO.PandocWriterConfig mFP tv oF = pandocWriterConfig config
  in  consumeKnitEffectStack config . KD.toDocListWithM
        (\(KP.PandocInfo _ tv') a ->
          fmap BH.renderHtml
            . KO.toBlazeDocument (KO.PandocWriterConfig mFP (tv' <> tv) oF)
            $ a
        )

-- | Create HTML Text from pandoc fragments
-- In use, you may need a type-application to specify m.
-- This allows use of any underlying monad to handle the Pandoc effects.
-- NB: Resulting document is *Lazy* Text, as produced by the Blaze render function.
knitHtml
  :: MonadIO m
  => KnitConfig
  -> P.Sem (KnitEffectDocStack m) ()
  -> m (Either PA.PandocError TL.Text)
knitHtml config =
  fmap (fmap (fmap BH.renderHtml)) (consumeKnitEffectStack config)
    . KO.pandocWriterToBlazeDocument (pandocWriterConfig config)

-- | Constraints required to knit a document using effects from a base monad m.
type KnitBase m effs = (MonadIO m, P.Member (P.Embed m) effs)

-- | lift an action in a base monad into a Polysemy monad.  This is just a renaming for convenience.
liftKnit :: P.Member (P.Embed m) r => m a -> P.Sem r a
liftKnit = P.embed

type KnitCache =  KC.AtomicCache T.Text (Streamly.SerialT Identity Word.Word8)

-- | Constraint alias for the effects we need (and run)
-- when calling Knit.
-- Anything inside a call to Knit can use any of these effects.
-- Any other effects will need to be run before @knitHtml(s)@
type KnitEffects r = (KPM.PandocEffects r
                     , P.Members [ KUI.UnusedId
                                 , KLog.Logger KLog.LogEntry
                                 , KLog.PrefixLog
                                 , P.Async
                                 , KnitCache
                                 , PE.Error KC.CacheError
                                 , PE.Error PA.PandocError
                                 , P.Embed IO] r
                     )

-- | Constraint alias for the effects we need to knit one document
type KnitOne r = (KnitEffects r, P.Member KP.ToPandoc r)

-- | Constraint alias for the effects we need to knit multiple documents.
type KnitMany r = (KnitEffects r, P.Member KP.Pandocs r)


-- From here down is unexported.  
-- | The exact stack we are interpreting when we knit
#if MIN_VERSION_pandoc(2,8,0)
type KnitEffectStack m
  = '[ KUI.UnusedId
     , KPM.Template
     , KPM.Pandoc
     , KnitCache
     , KLog.Logger KLog.LogEntry
     , KLog.PrefixLog
     , P.Async
     , PE.Error IOError
     , PE.Error KC.CacheError
     , PE.Error PA.PandocError
     , P.Embed IO
     , P.Embed m
     , P.Final m]
#else
type KnitEffectStack m
  = '[ KUI.UnusedId
     , KPM.Pandoc
     , KnitCache
     , KLog.Logger KLog.LogEntry
     , KLog.PrefixLog
     , P.Async
--     , PE.Error Exceptions.SomeException
     , PE.Error IOError
     , PE.Error KC.CacheError
     , PE.Error PA.PandocError
     , P.Embed IO
     , P.Embed m
     , P.Final m]
#endif

-- | Add a Multi-doc writer to the front of the effect list
type KnitEffectDocsStack m = (KP.Pandocs ': KnitEffectStack m)

-- | Add a single-doc writer to the front of the effect list
type KnitEffectDocStack m = (KP.ToPandoc ': KnitEffectStack m)

-- | run all knit-effects in @KnitEffectStack m@
#if MIN_VERSION_pandoc(2,8,0)
consumeKnitEffectStack
  :: forall m a
   . MonadIO m
  => KnitConfig
  -> P.Sem (KnitEffectStack m) a
  -> m (Either PA.PandocError a)
consumeKnitEffectStack config =
  P.runFinal
  . P.embedToFinal
  . PI.embedToMonadIO @m -- interpret (Embed IO) using m
  . PE.runError @KPM.PandocError
  . PE.mapError cacheErrorToPandocError
  . PE.mapError ioErrorToPandocError -- (\e -> PA.PandocSomeError ("Exceptions.Exception thrown: " <> (T.pack $ show e)))
  . P.asyncToIO -- this has to run after (above) the log, partly so that the prefix state is thread-local.
  . KLog.filteredAsyncLogEntriesToIO (logIf config)
  . KC.runPersistenceBackedAtomicInMemoryCache' 
  (KC.persistAsByteStreamly
    (\t -> T.unpack (cacheDir config <> "/" <> t))
  )
  . KPM.interpretInIO -- PA.PandocIO
  . KPM.interpretTemplateIO    
  . KUI.runUnusedId
  . maybe id KLog.wrapPrefix (outerLogPrefix config)
#else
consumeKnitEffectStack
  :: forall m a
   . MonadIO m
  => KnitConfig
  -> P.Sem (KnitEffectStack m) a
  -> m (Either PA.PandocError a)
consumeKnitEffectStack config =
  P.runFinal
  . P.embedToFinal
  . PI.embedToMonadIO @m -- interpret (Embed IO) using m
  . PE.runError
  . PE.mapError cacheErrorToPandocError
  . PE.mapError ioErrorToPandocError -- (\e -> PA.PandocSomeError ("Exceptions.Exception thrown: " <> (T.pack $ show e)))
  . P.asyncToIO -- this has to run after (above) the log, partly so that the prefix state is thread-local.
  . KLog.filteredAsyncLogEntriesToIO (logIf config)
  . KC.runPersistentBackedAtomicInmemoryCache'
  (KC.persistAsByteStreamly
    (\t -> T.unpack (cacheDir config <> "/" <> t))
  )
  . KPM.interpretInIO -- PA.PandocIO        
  . KUI.runUnusedId
  . maybe id KLog.wrapPrefix (outerLogPrefix config)
#endif    


ioErrorToPandocError :: IE.IOError -> KPM.PandocError
ioErrorToPandocError e = PA.PandocIOError (KPM.textToPandocText $ ("IOError: " <> (T.pack $ show e))) e
{-# INLINEABLE ioErrorToPandocError #-}

cacheErrorToPandocError :: KC.CacheError -> KPM.PandocError
cacheErrorToPandocError e = PA.PandocSomeError (KPM.textToPandocText $ ("CacheError: " <> (T.pack $ show e)))
{-# INLINEABLE cacheErrorToPandocError #-}
