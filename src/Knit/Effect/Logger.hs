{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.Logger
Description : Polysemy logging effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

<https://github.com/isovector/polysemy#readme Polysemy> logger effect,
using pretty-printing and severity based on <http://hackage.haskell.org/package/logging-effect logging-effect>. Adds a Prefixing effect so that it's easy to wrap entire
functions, etc. in logging prefixes and thus to distinguish where things are being logged from more easily.  Also allows filtering
by severity.
-}
module Knit.Effect.Logger
  (
    -- * Logging Types
    LogSeverity(..)
  , LogEntry(..)
  , LogCat(..)

  -- * Effects
  , Logger(..)
  , PrefixLog
  , CatSeverityState

  -- * Actions
  , log
  , logLE
  , logCat
  , wrapPrefix
  , getPrefix
  , logWithPrefixToIO
  , adjCatSeverity
  , withCatSeverity

  -- * Interpreters
  , filteredLogEntriesToIO
  , filteredLogEntriesToColorizedIO
  , interpretLogCatWithLogLE
  , interpretCatSeverityState

  -- * Subsets for filtering
  , logAll
  , logDebug
  , logDiagnostic
  , nonDiagnostic

  -- * Type Synonyms and Constraints for convenience
  , PrefixedLogEffects
  , PrefixedLogEffectsLE
  , LogWithPrefixes
  , LogWithPrefixesLE
  , LogWithPrefixesCat
  , LogWithPrefixIO
  -- * Library logging
--  , khDebugLog
  )
where

import Prelude hiding (hFlush)
import qualified Polysemy                      as P

import           Polysemy.Internal              ( send )
import qualified Polysemy.AtomicState          as P
import qualified Polysemy.State          as P

import qualified Data.IORef                    as IORef
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import qualified Prettyprinter     as PP
import qualified Prettyprinter.Render.Text
                                               as PP
import qualified Text.Colour                   as TC
import           Data.Data (Data)

import           System.IO                      ( hFlush ) -- relude claims to export this but seems not to?

import qualified Say                           as S



-- TODO: consider a more interesting Handler type.  As in co-log (https://hackage.haskell.org/package/co-log-core)
-- where you newtype it and then can exploit its profunctoriality.
-- I got lost here.  Was trying to be compatible with logging-effect but I'm not sure why
-- the idea that the runner takes a function to handle the logging in the rest of the stack seems okay.  Though why not be more direct
-- once we are using effects in the first place?  Isn't that handler a mix of pretty-printing and interpreting and we would
-- rather separate those concerns?  So we should have an (a -> Text) and then interpreters in whatever?  I guess we merge them because
-- conversion is uneccessary if we throw a message away?  But still, the interpreters could take the pretty-printers as arguments?
-- Parking this for now, since it has absorbed outsize time for no benefit except some understanding.

-- | Severity/importance of message.
data LogSeverity =
  -- | Most detailed levels of logging.  Int argument can be used adding fine distinctions between debug levels.
  Debug Int
  -- | Minimal details about effects and what is being called.
  | Diagnostic
  -- | Informational messages about progress of compuation or document knitting.
  | Info
  -- | Informational but likely resulting from a specially set severity for debugging one component
  | Special
  -- | Messages intended to alert the user to an issue in the computation or document production.
  | Warning
  -- | Likely unrecoverable issue in computation or document production.
  | Error
  deriving (Show, Eq, Ord, Typeable, Data)

-- NB: Cribbed from monad-logger.  Thanks ocharles!
-- TODO: add colors for ansi-terminal output
instance PP.Pretty LogSeverity where
  pretty = PP.pretty @Text . show

-- | A basic log entry with a severity and a ('Text') message
data LogEntry = LogEntry { severity :: !LogSeverity, message :: ~T.Text }

-- | log with a category and a base severity. We can raise that severity but not lower it
data LogCat = LogCat { logCategory :: !Text, lcSeverity :: !LogSeverity, lcMessage :: ~T.Text }


-- | log everything.
logAll :: LogSeverity -> Bool
logAll = const True
{-# INLINEABLE logAll #-}

-- | log all but 'Debug' messages.
logDiagnostic :: LogSeverity -> Bool
logDiagnostic (Debug _) = False
logDiagnostic _         = True
{-# INLINEABLE logDiagnostic #-}

-- | log everything above 'Diagnostic'.
nonDiagnostic :: LogSeverity -> Bool
nonDiagnostic ls = ls `elem` [Info, Special, Warning, Error]
{-# INLINEABLE nonDiagnostic #-}

-- | log debug messages with level lower than or equal to the given @Int@.
logDebug :: Int -> LogSeverity -> Bool
logDebug l (Debug n) = n <= l
logDebug _ _         = True
{-# INLINEABLE logDebug #-}

-- | The Logger effect (the same as the 'Polysemy.Output' effect). But adds a stored map of keyed severities for shifting log levels of components.
data Logger a m r where
  Log :: a -> Logger a m ()

-- | Add one log entry of arbitrary type.  If you want to log with another type besides @LogEntry.
log :: P.Member (Logger a) effs => a -> P.Sem effs ()
log = send . Log
{-# INLINEABLE log #-}


--type CatSeverity c = c -> LogSeverity
type CatSeverityState = P.AtomicState (Map.Map Text LogSeverity)

adjCatSeverity :: P.Member CatSeverityState r => Text -> LogSeverity -> P.Sem r (Map.Map Text LogSeverity)
adjCatSeverity c ls = do
  oldMap <- P.atomicGet
  let newMap = Map.insert c ls oldMap
  P.atomicPut newMap
  pure oldMap

withCatSeverity :: forall r x . P.Member CatSeverityState r => Text -> LogSeverity -> P.Sem r x -> P.Sem r x
withCatSeverity c ls mx = do
  oldF <- adjCatSeverity c ls
  x <- mx
  P.atomicPut oldF
  pure x

-- | Add one log-entry of the @LogEntry@ type.
logLE :: P.Member (Logger LogEntry) effs => LogSeverity -> T.Text -> P.Sem effs ()
logLE ls lm = log (LogEntry ls lm)
{-# INLINEABLE logLE #-}

-- | Add one log-entry of the @LogEntry@ type with severity keyed by the given category
logCat :: P.Member (Logger LogCat) effs => Text -> LogSeverity -> T.Text -> P.Sem effs ()
logCat cat sev msg = log (LogCat cat sev msg)
{-# INLINEABLE logCat #-}


-- | Type-alias for handler functions (unexported).
type Handler m msg = msg -> m ()

-- | Helper function for logging with monad-logger handler.
logWithHandler
  :: Handler (P.Sem effs) a -> P.Sem (Logger a ': effs) x -> P.Sem effs x
logWithHandler handler = P.interpret (\(Log a) -> handler a)
{-# INLINEABLE logWithHandler #-}

-- | Prefix Effect
data PrefixLog m r where
  AddPrefix :: T.Text -> PrefixLog m () -- ^ Represents adding a prefix to the logging output
  RemovePrefix :: PrefixLog m () -- ^ Represents removing one level of prefixing
  GetPrefix :: PrefixLog m T.Text -- ^ Represents retrieving the current prefix

-- | Add one level of prefix.
addPrefix :: P.Member PrefixLog effs => T.Text -> P.Sem effs ()
addPrefix = send . AddPrefix
{-# INLINEABLE addPrefix #-}

-- | Remove last prefix.
removePrefix :: P.Member PrefixLog effs => P.Sem effs ()
removePrefix = send RemovePrefix
{-# INLINEABLE removePrefix #-}

-- | Get current prefix
getPrefix :: P.Member PrefixLog effs => P.Sem effs T.Text
getPrefix = send GetPrefix
{-# INLINEABLE getPrefix #-}

-- | Add a prefix for the block of code.
wrapPrefix :: P.Member PrefixLog effs => T.Text -> P.Sem effs a -> P.Sem effs a
wrapPrefix p l = do
  addPrefix p
  res <- l
  removePrefix
  return res
{-# INLINEABLE wrapPrefix #-}

-- | Interpret PrefixLog in @Polysemy.State [T.Text]@.
prefixInState
  :: forall effs a
   . P.Sem (PrefixLog ': effs) a
  -> P.Sem (P.State [T.Text] ': effs) a
prefixInState = P.reinterpret $ \case
  AddPrefix t  -> P.modify (t :)
  RemovePrefix -> P.modify @[T.Text] (fromMaybe [] . viaNonEmpty tail) -- type application required here since tail is polymorphic
  GetPrefix    -> fmap (T.intercalate "." . List.reverse) P.get
{-# INLINEABLE prefixInState #-}

-- | Interpret the 'PrefixLog' effect in State and run that.
runPrefix :: P.Sem (PrefixLog ': effs) a -> P.Sem effs a
runPrefix = fmap snd . P.runState [] . prefixInState
{-# INLINEABLE runPrefix #-}

-- | Monad-logger style wrapper to add prefixes to log messages.
data WithPrefix a = WithPrefix { msgPrefix :: T.Text, discardPrefix :: a }
data WithSeverity a = WithSeverity { msgSeverity :: LogSeverity, discardSeverity :: a }

-- | Render a prefixed log message with the pretty-printer.
renderWithPrefix :: (a -> PP.Doc ann) -> WithPrefix a -> PP.Doc ann
renderWithPrefix k (WithPrefix pr a) = PP.pretty pr PP.<+> PP.align (k a)
{-# INLINEABLE renderWithPrefix #-}

-- | Render a prefixed log message with the pretty-printer.
renderLogEntry
  :: (T.Text -> PP.Doc ann) -> (LogEntry -> PP.Doc ann)
renderLogEntry k (LogEntry s t) =
  PP.brackets (PP.pretty s) PP.<+> PP.align (k t)


-- | Use @PrefixLog@ Effect to re-interpret all the logged messages to WithPrefix form.
logPrefixed
  :: P.Member PrefixLog effs
  => P.Sem (Logger a ': effs) x
  -> P.Sem (Logger (WithPrefix a) ': effs) x
logPrefixed =
  P.reinterpret (\(Log a) -> getPrefix >>= (\p -> log (WithPrefix p a)))
{-# INLINEABLE logPrefixed #-}

-- the use of "raise" below is there since we are running the handler in the stack that still has the LogPrefix effect.
-- I couldn't figure out how to write this the other way.
-- | Given a handler for @WithPrefix a@ in the remaining effects (IO, e.g.,), run the Logger and Prefix effects and handle all the logging
-- messages via that handler.
logAndHandlePrefixed
  :: forall effs a x
   . Handler (P.Sem effs) (WithPrefix a)
  -> P.Sem (Logger a ': (PrefixLog ': effs)) x
  -> P.Sem effs x
logAndHandlePrefixed handler =
  runPrefix
    . logWithHandler (P.raise . handler)
    . logPrefixed @(PrefixLog ': effs)
{-# INLINEABLE logAndHandlePrefixed #-}

-- | Add a severity filter to a handler.
filterLog :: Monad m => (a -> Bool) -> Handler m a -> Handler m a
filterLog filterF h a = when (filterF a) $ h a
{-# INLINEABLE filterLog #-}

-- | Simple handler, uses a function from message to Text and then outputs all messages in 'IO'.
-- Uses "Say" to insure messages issued from each thread are output coherently.
-- Can be used as base for any other handler that gives @Text@.
logToIO :: MonadIO m => (a -> T.Text) -> Handler m a
logToIO asText a = liftIO $ do
  S.say $ asText a
  hFlush stdout
{-# INLINEABLE logToIO #-}

-- | Simple handler, uses a function from message to Text and then outputs all messages in 'IO'.
-- Uses "Say" to insure messages issued from each thread are output coherently.
-- Can be used as base for any other handler that gives @Text@.
logToColorizedIO :: MonadIO m => (a -> [TC.Chunk]) -> Handler m a
logToColorizedIO asChunks a = liftIO $ do
#if MIN_VERSION_streamly(0,9,0)
  TC.putChunksLocaleWith TC.With24BitColours $ asChunks a
#else
  TC.putChunksWith TC.With24BitColours $ asChunks a
#endif
  hFlush stdout
{-# INLINEABLE logToColorizedIO #-}


-- | '(a -> Text)' function for prefixedLogEntries
prefixedLogEntryToText :: WithPrefix LogEntry -> T.Text
prefixedLogEntryToText =
  PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . renderWithPrefix
    (renderLogEntry PP.pretty)
{-# INLINEABLE prefixedLogEntryToText #-}

-- | '(a -> [TC.Chunk])' function for prefixedLogEntries
prefixedLogEntryToColorizedChunks :: WithPrefix LogEntry -> [TC.Chunk]
prefixedLogEntryToColorizedChunks x =
  let prefix = msgPrefix x
      sev = severity $ discardPrefix x
      msg = message $ discardPrefix x
      plainChunk = TC.chunk
      sevChunk = case sev of
            Error -> TC.fore TC.brightMagenta $ TC.bold $ TC.chunk "Error"
            Warning -> TC.fore TC.brightYellow $ TC.bold $ TC.chunk "Warning"
            Special -> TC.fore TC.brightRed $ TC.bold $ TC.chunk "Special"
            Info -> TC.fore TC.green $ TC.chunk "Info"
            Diagnostic -> TC.fore TC.cyan $ TC.chunk "Diagnostic"
            Debug n -> TC.fore TC.blue $ TC.chunk $ "Debug " <> show n
      sevChunks = [TC.chunk "[",  sevChunk, TC.chunk "]"]
      prefixChunk = TC.chunk $ "(" <> prefix <> ")"
      msgChunk = TC.chunk msg
  in sevChunks <> [plainChunk " "] <> [prefixChunk] <> [plainChunk " "] <> [msgChunk] <> [TC.chunk "\n"]

--  PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . renderWithPrefix
--    (renderLogEntry PP.pretty)
{-# INLINEABLE prefixedLogEntryToColorizedChunks #-}


-- | log prefixed entries directly to IO
prefixedLogEntryToIO :: MonadIO m => Handler m (WithPrefix LogEntry)
prefixedLogEntryToIO = logToIO prefixedLogEntryToText
{-# INLINEABLE prefixedLogEntryToIO #-}

-- | log prefixed entries directly to IO
prefixedLogEntryToColorizedIO :: MonadIO m => Handler m (WithPrefix LogEntry)
prefixedLogEntryToColorizedIO = logToColorizedIO prefixedLogEntryToColorizedChunks
{-# INLINEABLE prefixedLogEntryToColorizedIO #-}


-- | This function can be used to log directly to IO, bypassing the effect.
-- It's here to allow logging from within functions that must be run under more
-- limited stacks and then embedded.
logWithPrefixToIO :: LogWithPrefixIO
logWithPrefixToIO prefix le = let wp = WithPrefix prefix le in prefixedLogEntryToIO wp
{-# INLINEABLE logWithPrefixToIO #-}

-- | A synonym for a function to handle direct logging from IO.  Used to allow logging from any stack with IO.
type LogWithPrefixIO = T.Text -> LogEntry -> IO ()

-- | Run the 'Logger' and 'PrefixLog' effects in 'IO': filtered via the severity of the message and formatted using "prettyprinter".
filteredLogEntriesToIO
  :: MonadIO (P.Sem r)
  =>  (LogSeverity -> Bool)
  -> P.Sem (Logger LogEntry ': (PrefixLog ': r)) x
  -> P.Sem r x
filteredLogEntriesToIO lsF mx = do
  let f a = lsF (severity $ discardPrefix a)
  logAndHandlePrefixed (filterLog f prefixedLogEntryToIO) mx
{-# INLINEABLE filteredLogEntriesToIO #-}

-- | Run the 'Logger' and 'PrefixLog' effects in 'IO': filtered via the severity of the message and formatted using "prettyprinter".
filteredLogEntriesToColorizedIO
  :: MonadIO (P.Sem r)
  => (LogSeverity -> Bool)
  -> P.Sem (Logger LogEntry ': (PrefixLog ': r)) x
  -> P.Sem r x
filteredLogEntriesToColorizedIO lsF mx = do
  let f a = lsF (severity $ discardPrefix a)
  logAndHandlePrefixed (filterLog f prefixedLogEntryToColorizedIO) mx
{-# INLINEABLE filteredLogEntriesToColorizedIO #-}

-- | interpret a LogCat effect in terms of a LogEntry effect given the category to severity ands category to Text functions
interpretLogCatWithLogLE :: (P.Members [CatSeverityState, Logger LogEntry] r)
                         => P.InterpreterFor (Logger LogCat) r
interpretLogCatWithLogLE = P.interpret $ \case
  Log (LogCat c sev msg) -> do
    lsM <- (Map.lookup c) <$> P.atomicGet
    let ls' = fromMaybe sev $ fmap (max sev) lsM
    logLE ls' $ c <> ": " <> msg
{-# INLINEABLE interpretLogCatWithLogLE #-}

interpretCatSeverityState :: P.Member (P.Embed IO) r
                          => (Map.Map Text LogSeverity) -> P.InterpreterFor CatSeverityState r
interpretCatSeverityState catLS mx = do
    ioRef <- P.embed $ IORef.newIORef catLS
    P.runAtomicStateIORef ioRef mx
{-# INLINEABLE interpretCatSeverityState #-}


{-
logCatToLogLE
  :: forall c r . P.Members [P.Embed IO, Logger LogEntry] r
  => (c -> LogSeverity)
  -> (c -> Text)
  -> P.InterpreterFor (Logger (LogCat c)) r
logCatToLogLE catLS catText mx = do
  ioRef <- P.embed $ IORef.newIORef catLS
  P.runAtomicStateIORef ioRef $ interpretLogCatWithLogLE catText $ P.raiseUnder mx
{-# INLINEABLE logCatToLogLE #-}
-}

-- | List of Logger effects for a prefixed log of type @a@
type PrefixedLogEffects a = [PrefixLog, Logger a]

-- | List of Logger effects for a prefixed log of type @LogEntry@
type PrefixedLogEffectsLE = PrefixedLogEffects LogEntry

-- | List of Logger effects for a prefixed log of type @LogEntry@
type PrefixedLogEffectsCat = [PrefixLog, Logger LogCat, CatSeverityState]

-- | Constraint helper for logging with prefixes
type LogWithPrefixes a effs = P.Members (PrefixedLogEffects a) effs --(P.Member PrefixLog effs, P.Member (Logger a) effs)

-- | Constraint helper for @LogEntry@ type with prefixes
type LogWithPrefixesLE effs = LogWithPrefixes LogEntry effs --(P.Member PrefixLog effs, P.Member (Logger a) effs)

-- | Constraint helper for @LogEntry@ type with prefixes
type LogWithPrefixesCat effs = P.Members PrefixedLogEffectsCat effs
