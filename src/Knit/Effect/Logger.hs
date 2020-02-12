{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
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

  -- * Effects
  , Logger(..)
  , PrefixLog

  -- * Actions
  , log
  , logLE
  , wrapPrefix

  -- * Interpreters
  , filteredLogEntriesToIO
  , filteredAsyncLogEntriesToIO

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

  -- * Re-Exports
  , Sem
  , Member
  , Handler
  )
where

import qualified Polysemy                      as P
import           Polysemy                       ( Member
                                                , Sem
                                                )
import qualified Polysemy.Async                as P
import           Polysemy.Internal              ( send )
import qualified Polysemy.State                as P

import qualified Control.Concurrent.STM        as C
import qualified Control.Exception             as X
import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Log              ( Handler )
import qualified Control.Monad.Log             as ML
import qualified Data.List                     as List
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text
                                               as PP
import           Prelude                 hiding ( log )

import           System.IO                      ( hFlush
                                                , stdout
                                                )

import qualified Say                           as S



-- TODO: consider a more interesting Handler type.  As in co-log (https://hackage.haskell.org/package/co-log-core)
-- where you newtype it and then can exploit its profunctoriality.
-- I got lost here.  Was trying to be compatible with logging-effect but I'm not sure why
-- the idea that the runner takes a function to handle the logging in the rest of the stack seems okay.  Though why not be more direct
-- once we are using effects in the first place?  Isn't that handler a mix of pretty-printing and interpreting and we would
-- rather separate those concerns?  So we should have an (a -> Text) and then interpreters in whatever?  I guess we merge them because
-- conversion is uneccessary if we throw a message away?  But still, the interpreters could take the pretty-printers as arguments?
-- Parking this for now, since it has absorbed outsize time for no benefit except some understanding.

-- | Severity of message.  Based on monad-logger.
data LogSeverity = Debug Int | Diagnostic | Info | Warning | Error deriving (Show, Eq, Ord)

-- | Map between @LogSeverity@ and monad-logger severity.
logSeverityToSeverity :: LogSeverity -> ML.Severity
logSeverityToSeverity (Debug _)  = ML.Debug
logSeverityToSeverity Diagnostic = ML.Debug
logSeverityToSeverity Info       = ML.Informational
logSeverityToSeverity Warning    = ML.Warning
logSeverityToSeverity Error      = ML.Error

-- | A basic LogEntry with a severity and a (Text) message
data LogEntry = LogEntry { severity :: LogSeverity, message :: T.Text }

-- | Convert @LogEntry@ to monad-logger style.
logEntryToWithSeverity :: LogEntry -> ML.WithSeverity T.Text
logEntryToWithSeverity (LogEntry s t) =
  ML.WithSeverity (logSeverityToSeverity s) t


-- | log everything.
logAll :: LogSeverity -> Bool
logAll = const True

-- | log all but 'Debug' messages
logDiagnostic :: LogSeverity -> Bool
logDiagnostic (Debug _) = False
logDiagnostic _         = True

-- | log everything above 'Diagnostic'
nonDiagnostic :: LogSeverity -> Bool
nonDiagnostic ls = ls `elem` [Info, Warning, Error]

-- | log debug messages with level lower than or equal to l
logDebug :: Int -> LogSeverity -> Bool
logDebug l (Debug n) = n <= l
logDebug _ _         = True

-- | The Logger effect
data Logger a m r where
  Log :: a -> Logger a m ()

-- | Add one log entry of arbitrary type.  If you want to log with another type besides @LogEntry.
log :: P.Member (Logger a) effs => a -> P.Sem effs ()
log = send . Log

-- | Add one log-entry of the @LogEntry@ type.
logLE
  :: P.Member (Logger LogEntry) effs => LogSeverity -> T.Text -> P.Sem effs ()
logLE ls lm = log (LogEntry ls lm)

-- | Helper function for logging with monad-logger handler.
logWithHandler
  :: Handler (P.Sem effs) a -> P.Sem (Logger a ': effs) x -> P.Sem effs x
logWithHandler handler = P.interpret (\(Log a) -> handler a)

-- | Prefix Effect
data PrefixLog m r where
  AddPrefix :: T.Text -> PrefixLog m () -- ^ Represents adding a prefix to the logging output
  RemovePrefix :: PrefixLog m () -- ^ Represents removing one level of prefixing
  GetPrefix :: PrefixLog m T.Text -- ^ Represents retrieving the current prefix

-- | Add one level of prefix.
addPrefix :: P.Member PrefixLog effs => T.Text -> P.Sem effs ()
addPrefix = send . AddPrefix

-- | Remove last prefix.
removePrefix :: P.Member PrefixLog effs => P.Sem effs ()
removePrefix = send RemovePrefix

-- | Get current prefix 
getPrefix :: P.Member PrefixLog effs => P.Sem effs T.Text
getPrefix = send $ GetPrefix

-- | Add a prefix for the block of code.
wrapPrefix :: P.Member PrefixLog effs => T.Text -> P.Sem effs a -> P.Sem effs a
wrapPrefix p l = do
  addPrefix p
  res <- l
  removePrefix
  return res

-- | Interpret LogPrefix in @Polysemy.State [T.Text]@.
prefixInState
  :: forall effs a
   . P.Sem (PrefixLog ': effs) a
  -> P.Sem (P.State [T.Text] ': effs) a
prefixInState = P.reinterpret $ \case
  AddPrefix t  -> P.modify (t :)
  RemovePrefix -> P.modify @[T.Text] tail -- type application required here since tail is polymorphic
  GetPrefix    -> fmap (T.intercalate "." . List.reverse) P.get

-- | Interpret the 'LogPrefix' effect in State and run that.
runPrefix :: P.Sem (PrefixLog ': effs) a -> P.Sem effs a
runPrefix = fmap snd . P.runState [] . prefixInState

-- | Monad-logger style wrapper to add prefixes to log messages.
data WithPrefix a = WithPrefix { msgPrefix :: T.Text, discardPrefix :: a }

-- | Render a prefixed log message with the pretty-printer.
renderWithPrefix :: (a -> PP.Doc ann) -> WithPrefix a -> PP.Doc ann
renderWithPrefix k (WithPrefix pr a) = PP.pretty pr PP.<+> PP.align (k a)

-- | Use @PrefixLog@ Effect to re-interpret all the logged messages to WithPrefix form.
logPrefixed
  :: P.Member PrefixLog effs
  => P.Sem (Logger a ': effs) x
  -> P.Sem (Logger (WithPrefix a) ': effs) x
logPrefixed =
  P.reinterpret (\(Log a) -> getPrefix >>= (\p -> log (WithPrefix p a)))

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

-- | Add a severity filter to a handler.
filterLog :: Monad m => (a -> Bool) -> Handler m a -> Handler m a
filterLog filterF h a = when (filterF a) $ h a

-- | Simple handler, uses a function from message to Text and then outputs all messages in IO.
-- Can be used as base for any other handler that gives @Text@.
logToIO :: MonadIO m => (a -> T.Text) -> Handler m a
logToIO toText a = liftIO $ do
  S.say $ toText a
  hFlush stdout

data NextOrDone = Next T.Text | Done -- isomorphic to (Maybe T.Text)

-- | log to an STM TChan.  This allows logging from different threads to log each message atomically
logToTChan :: MonadIO m => C.TChan NextOrDone -> (a -> T.Text) -> Handler m a
logToTChan ch toText =
  liftIO . C.atomically . C.writeTChan ch . (Next . toText)

-- | '(a -> Text)' function for prefixedLogEntries
prefixedLogEntryToText :: WithPrefix LogEntry -> T.Text
prefixedLogEntryToText =
  (PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . renderWithPrefix
    (ML.renderWithSeverity PP.pretty . logEntryToWithSeverity)
  )

-- | log prefixed entries directly to IO
prefixedLogEntryToIO :: MonadIO m => Handler m (WithPrefix LogEntry)
prefixedLogEntryToIO = logToIO prefixedLogEntryToText

-- | log prefixed entries to given TChan
prefixedLogEntryToTChan
  :: MonadIO m => C.TChan NextOrDone -> Handler m (WithPrefix LogEntry)
prefixedLogEntryToTChan ch = logToTChan ch prefixedLogEntryToText

-- | Run the Logger and PrefixLog effects using the preferred handler and filter output in any Polysemy monad with IO in the union.
filteredLogEntriesToIO
  :: MonadIO (P.Sem r)
  => (LogSeverity -> Bool)
  -> P.Sem (Logger LogEntry ': (PrefixLog ': r)) x
  -> P.Sem r x
filteredLogEntriesToIO lsF mx = do
  let f a = lsF (severity $ discardPrefix a)
  logAndHandlePrefixed (filterLog f $ prefixedLogEntryToIO) mx

filteredAsyncLogEntriesToIO
  :: (MonadIO (P.Sem r), P.Member P.Async r)
  => (LogSeverity -> Bool)
  -> P.Sem (Logger LogEntry ': (PrefixLog ': r)) x
  -> P.Sem r x
filteredAsyncLogEntriesToIO lsF mx = do
  ch            <- liftIO $ C.atomically C.newTChan -- create a TChan for logging messages
  loggingThread <- P.async $ liftIO $ printNextUntilDone ch -- launch a thread for printing them
  res           <- logAndHandlePrefixed
    (filterLog (lsF . severity . discardPrefix) $ prefixedLogEntryToTChan ch)
    mx
  liftIO $ C.atomically $ C.writeTChan ch Done -- tell the printing thread to finish
  _ <- P.await loggingThread -- wait until it finishes printing any remaining messages
  return res

-- | Printing loop. Wait for the next message on the channel
-- If it's a 'Next Text' print the text and loop.
-- If it's a 'Done' then exit
printNextUntilDone :: C.TChan NextOrDone -> IO ()
printNextUntilDone ch = do
  nOrd <- C.atomically $ C.readTChan ch
  case nOrd of
    Next t -> S.say t >> hFlush stdout >> printNextUntilDone ch
    Done   -> return ()

{-
withAsyncLogging :: P.Members '[PrefixLog, P.Async] r => P.Sem r a -> P.Sem r a
withAsyncLogging = P.interceptH $ \case
  P.Async x ->
    P.Async
      $   (liftIO C.myThreadId)
      >>= (\ti -> wrapPrefix ("(ThreadID=" <> (T.pack $ show ti)) x)
-}

-- | List of Logger effects for a prefixed log of type @a@
type PrefixedLogEffects a = [PrefixLog, Logger a]

-- | List of Logger effects for a prefixed log of type @LogEntry@
type PrefixedLogEffectsLE = PrefixedLogEffects LogEntry

-- | Constraint helper for logging with prefixes
type LogWithPrefixes a effs = P.Members (PrefixedLogEffects a) effs --(P.Member PrefixLog effs, P.Member (Logger a) effs)

-- | Constraint helper for @LogEntry@ type with prefixes
type LogWithPrefixesLE effs = LogWithPrefixes LogEntry effs --(P.Member PrefixLog effs, P.Member (Logger a) effs)

