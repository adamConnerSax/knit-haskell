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

    -- * Knit documents
  , knitHtml
  , knitHtmls

    -- * helpers
  , liftKnit

    -- * Constraints for knit-haskell actions (see examples)
  , KnitEffects
  , CacheEffects
  , CacheEffectsD
  , KnitOne
  , KnitMany
  , KnitBase

    -- * Debug/Testing helpers
  , consumeKnitEffectStack
  )
where

import qualified Control.Monad.Catch as Exceptions (SomeException, displayException)
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Polysemy                      as P
import qualified Polysemy.Async                as P
import qualified Polysemy.Error                as PE
import qualified Polysemy.IO                   as PI
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
import qualified Knit.Effect.Serialize         as KS

{- |
Parameters for knitting. If possible, create this via, e.g.,

@
myConfig = (defaultKnitConfig $ Just "myCacheDir") { pandocWriterConfig = myConfig }
@
so that your code will still compile if parameters are added to this structure.

NB: the type parameters of this configuration specify the cache types:

- @sc :: Type -> Constraint@, where @c a@ is the constraint to be satisfied for serializable @a@.
- @ct :: Type@, is the value type held in the in-memory cache.
- @k  :: Type@, is the key type of the cache.

The @serializeDict@ field holds functions for encoding (@forall a. c a=> a -> ct@)
and decoding (@forall a. c a => ct -> Either SerializationError a@).

The @persistCache@ field holds an interpreter for the persistence layer of
the cache. See 'Knit.AtomicCache' for examples of persistence layers.

If you want to use a different serializer ("binary" or "store") and/or a different type to hold cached
values in-memory, you can set these fields accordingly.
-}
data KnitConfig sc ct k = KnitConfig { outerLogPrefix :: Maybe T.Text
                                     , logIf :: KLog.LogSeverity -> Bool
                                     , pandocWriterConfig :: KO.PandocWriterConfig
                                     , serializeDict :: KS.SerializeDict sc ct
                                     , persistCache :: forall r. (P.Member (P.Embed IO) r
                                                                 , P.Member (PE.Error KC.CacheError) r
                                                                 , KLog.LogWithPrefixesLE r)
                                                    => P.InterpreterFor (KC.Cache k ct) r
                                     }


-- | Sensible defaults for a knit configuration.
defaultKnitConfig :: Maybe T.Text -- ^ Optional cache-directory.  Defaults to ".knit-haskell-cache".
                  -> KnitConfig KS.DefaultSerializer KS.DefaultCacheData T.Text -- ^ configuration
defaultKnitConfig cacheDirM =
  let cacheDir = fromMaybe ".knit-haskell-cache" cacheDirM
  in KnitConfig
     (Just "knit-haskell")
     KLog.nonDiagnostic
     (KO.PandocWriterConfig Nothing M.empty id)
     KS.cerealStreamlyDict
     (KC.persistStreamlyByteArray (\t -> toString (cacheDir <> "/" <> t)))
{-# INLINEABLE defaultKnitConfig #-}

-- | Create multiple HTML docs (as Text) from the named sets of pandoc fragments.
-- In use, you may need a type-application to specify @m@.
-- This allows use of any underlying monad to handle the Pandoc effects.
-- NB: Resulting documents are *Lazy* Text, as produced by the Blaze render function.
knitHtmls
  :: (MonadIO m, Ord k, Show k)
  => KnitConfig c ct k -- ^ configuration
  -> P.Sem (KnitEffectDocsStack c ct k m) () -- ^ computation producing a list of documents
  -> m (Either PA.PandocError [KP.DocWithInfo KP.PandocInfo TL.Text]) -- ^ Resulting docs or error, in base monad, usually IO.
knitHtmls config =
  let KO.PandocWriterConfig mFP tv oF = pandocWriterConfig config
  in  consumeKnitEffectStack config . KD.toDocListWithM
        (\(KP.PandocInfo _ tv') a ->
          fmap BH.renderHtml
            . KO.toBlazeDocument (KO.PandocWriterConfig mFP (tv' <> tv) oF)
            $ a
        )
{-# INLINEABLE knitHtmls #-}

-- | Create HTML Text from pandoc fragments.
-- In use, you may need a type-application to specify @m@.
-- This allows use of any underlying monad to handle the Pandoc effects.
-- NB: Resulting document is *Lazy* Text, as produced by the Blaze render function.
knitHtml
  :: (MonadIO m, Ord k, Show k)
  => KnitConfig c ct k -- ^ configuration
  -> P.Sem (KnitEffectDocStack c ct k m) () -- ^ computation producing a single document
  -> m (Either PA.PandocError TL.Text) -- ^ Resulting document or error, in base monad.  Usually IO.
knitHtml config =
  fmap BH.renderHtml <<$>> consumeKnitEffectStack config
    . KO.pandocWriterToBlazeDocument (pandocWriterConfig config)
{-# INLINEABLE knitHtml #-}

-- | Constraints required to knit a document using effects from a base monad m.
type KnitBase m effs = (MonadIO m, P.Member (P.Embed m) effs)

-- | lift an action in a base monad into a Polysemy monad.  This is just a renaming of `P.embed` for convenience.
liftKnit :: P.Member (P.Embed m) r => m a -> P.Sem r a
liftKnit = P.embed
{-# INLINE liftKnit #-}

-- | Constraint alias for the effects we need (and run)
-- when calling 'knitHtml' or 'knitHtmls'.
-- Anything inside a call to Knit can use any of these effects.
-- Any other effects added to this stack will need to be run before @knitHtml(s)@
type KnitEffects r = (KPM.PandocEffects r
                     , P.Members [ KUI.UnusedId
                                 , KLog.Logger KLog.LogEntry
                                 , KLog.PrefixLog
                                 , P.Async
                                 , PE.Error KC.CacheError
                                 , PE.Error Exceptions.SomeException
                                 , PE.Error PA.PandocError
                                 , P.Embed IO] r
                     )

-- | Constraint alias for the effects we need to use the cache.
type CacheEffects c ct k r = (P.Members [KS.SerializeEnv c ct, KC.Cache k ct] r)

-- | Constraint alias for the effects we need to use the default cache with @Text@ keys.
type CacheEffectsD r = CacheEffects KS.DefaultSerializer KS.DefaultCacheData T.Text r

-- | Constraint alias for the effects we need to knit one document.
type KnitOne r = (KnitEffects r, P.Member KP.ToPandoc r)

-- | Constraint alias for the effects we need to knit multiple documents.
type KnitMany r = (KnitEffects r, P.Member KP.Pandocs r)

-- From here down is unexported.
-- | The exact stack we are interpreting when we knit
#if MIN_VERSION_pandoc(2,8,0)
type KnitEffectStack c ct k m
  = '[ KUI.UnusedId
     , KPM.Template
     , KPM.Pandoc
     , KS.SerializeEnv c ct
     , KC.Cache k ct
     , KLog.Logger KLog.LogEntry
     , KLog.PrefixLog
     , P.Async
     , PE.Error IE.IOError
     , PE.Error KC.CacheError
     , PE.Error Exceptions.SomeException
     , PE.Error PA.PandocError
     , P.Embed IO
     , P.Embed m
     , P.Final m]
#else
type KnitEffectStack c ct k m
  = '[ KUI.UnusedId
     , KPM.Pandoc
     , KS.SerializeEnv c ct
     , KC.Cache k ct
     , KLog.Logger KLog.LogEntry
     , KLog.PrefixLog
     , P.Async
     , PE.Error IOError
     , PE.Error KC.CacheError
     , PE.Error Exceptions.SomeException
     , PE.Error PA.PandocError
     , P.Embed IO
     , P.Embed m
     , P.Final m]
#endif

-- | Add a Multi-doc writer to the front of the effect list
type KnitEffectDocsStack c ct k m = (KP.Pandocs ': KnitEffectStack c ct k m)

-- | Add a single-doc writer to the front of the effect list
type KnitEffectDocStack c ct k m = (KP.ToPandoc ': KnitEffectStack c ct k m)

-- | run all knit-effects in @KnitEffectStack m@
#if MIN_VERSION_pandoc(2,8,0)
consumeKnitEffectStack
  :: forall c ct k m a
   . (MonadIO m, Ord k, Show k)
  => KnitConfig c ct k
  -> P.Sem (KnitEffectStack c ct k m) a
  -> m (Either PA.PandocError a)
consumeKnitEffectStack config =
  P.runFinal
  . P.embedToFinal
  . PI.embedToMonadIO @m -- interpret (Embed IO) using m
  . PE.runError @KPM.PandocError
  . PE.mapError someExceptionToPandocError
  . PE.mapError cacheErrorToPandocError
  . PE.mapError ioErrorToPandocError -- (\e -> PA.PandocSomeError ("Exceptions.Exception thrown: " <> (T.pack $ show e)))
  . P.asyncToIO -- this has to run after (above) the log, partly so that the prefix state is thread-local.
  . KLog.filteredLogEntriesToColorizedIO (logIf config)
  . KC.runPersistenceBackedAtomicInMemoryCache' (persistCache config)
  . KS.runSerializeEnv (serializeDict config)
  . KPM.interpretInIO -- PA.PandocIO
  . KPM.interpretTemplateIO
  . KUI.runUnusedId
  . maybe id KLog.wrapPrefix (outerLogPrefix config)
#else
consumeKnitEffectStack
  :: forall c ct k m a
   . (MonadIO m, Ord k, Show k)
  => KnitConfig c ct k
  -> P.Sem (KnitEffectStack c ct k m) a
  -> m (Either PA.PandocError a)
consumeKnitEffectStack config =
  P.runFinal
  . P.embedToFinal
  . PI.embedToMonadIO @m -- interpret (Embed IO) using m
  . PE.runError
  . PE.mapError someExceptionToPandocError
  . PE.mapError cacheErrorToPandocError
  . PE.mapError ioErrorToPandocError -- (\e -> PA.PandocSomeError ("Exceptions.Exception thrown: " <> (T.pack $ show e)))
  . P.asyncToIO -- this has to run after (above) the log, partly so that the prefix state is thread-local.
  . KLog.filteredLogEntriesToColorizedIO (logIf config)
  . KC.runPersistenceBackedAtomicInMemoryCache' (persistCache config)
  . KS.runSerializeEnv (serializeDict config)
  . KPM.interpretInIO -- PA.PandocIO
  . KUI.runUnusedId
  . maybe id KLog.wrapPrefix (outerLogPrefix config)
#endif
{-# INLINEABLE consumeKnitEffectStack #-}


ioErrorToPandocError :: IE.IOError -> KPM.PandocError
ioErrorToPandocError e = PA.PandocIOError (KPM.textToPandocText ("IOError: " <> show e)) e
{-# INLINEABLE ioErrorToPandocError #-}

cacheErrorToPandocError :: KC.CacheError -> KPM.PandocError
cacheErrorToPandocError e = PA.PandocSomeError (KPM.textToPandocText ("CacheError: " <> show e))
{-# INLINEABLE cacheErrorToPandocError #-}

someExceptionToPandocError :: Exceptions.SomeException -> KPM.PandocError
someExceptionToPandocError = PA.PandocSomeError . KPM.textToPandocText . toText . Exceptions.displayException
{-# INLINEABLE someExceptionToPandocError #-}
