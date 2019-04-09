{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
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
Module      : Control.Monad.Freer.Loger
Description : freer-simple logging effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

freer-simple logger effect, using severity based on monad-logger. Adds a Prefixing effect so that it's easy to wrap entire
functions and more in logging prefixes and thus to distinguish where things are being logged from more easily.  Also allows filtering
by severity.
-}
module Control.Monad.Freer.Logger
  (
    -- * Logging Types
    LogSeverity(..)
  , LogEntry(..)
  -- * Effect Types
  , Logger(..)
  , PrefixLog
  -- * Combinators
  , log
  , logLE
  , wrapPrefix
  -- * subsets for filtering
  , logAll
  , nonDiagnostic
  -- * run, with prefixes, in IO
  , filteredLogEntriesToIO
  -- * Constraints for convenience 
  , PrefixedLogEffs
  , LogWithPrefixes
  -- * re-exports from freer-simple
  , Eff
  , Member
  , Handler
  )
where


import           Control.Monad.Freer            ( Eff
                                                , Member
                                                )
import qualified Control.Monad.Freer           as FR
import qualified Control.Monad.Freer.State     as FR
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Log              ( Handler )
import qualified Control.Monad.Log             as ML
import qualified Data.List                     as List
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text
                                               as PP
import           Prelude                 hiding ( log )


-- TODO: consider a more interesting Handler type.  As in co-log (https://hackage.haskell.org/package/co-log-core)
-- where you newtype it and then can exploit its profunctoriality.
-- I got lost here.  Was trying to be compatible with logging-effect but I'm not sure why
-- the idea that the runner takes a function to handle the logging in the rest of the stack seems okay.  Though why not be more direct
-- once we are using effects in the first place?  Isn't that handler a mix of pretty-printing and interpreting and we would
-- rather separate those concerns?  So we should have an (a -> Text) and then interpreters in whatever?  I guess we merge them because
-- conversion is uneccessary if we throw a message away?  But still, the interpreters could take the pretty-printers as arguments?
-- Parking this for now, since it has absorbed outsize time for no benefit except some understanding.


-- a simple type for logging text with a subset of severities
-- | Severity of message.  Based on monad-logger.
data LogSeverity = Diagnostic | Info | Warning | Error deriving (Show, Eq, Ord, Enum, Bounded)

-- | Map between LogSeverity and monad-logger severity
logSeverityToSeverity :: LogSeverity -> ML.Severity
logSeverityToSeverity Diagnostic = ML.Debug
logSeverityToSeverity Info       = ML.Informational
logSeverityToSeverity Warning    = ML.Warning
logSeverityToSeverity Error      = ML.Error

-- | A basic LogEntry with a severity and a (Text) message
data LogEntry = LogEntry { severity :: LogSeverity, message :: T.Text }

-- | convert @LogEntry to monad-logger style
logEntryToWithSeverity :: LogEntry -> ML.WithSeverity T.Text
logEntryToWithSeverity (LogEntry s t) =
  ML.WithSeverity (logSeverityToSeverity s) t

-- | format LogEntry for Text output
logEntryPretty :: LogEntry -> T.Text
logEntryPretty (LogEntry Diagnostic d) = "(Diagnostic): " <> d
logEntryPretty (LogEntry Info       t) = "(Info): " <> t
logEntryPretty (LogEntry Warning    w) = "(Warning): " <> w
logEntryPretty (LogEntry Error      e) = "(Error): " <> e

-- | filter which messages are ouptput.  Any severity in the list is included.
filterLogEntry :: [LogSeverity] -> LogEntry -> Maybe LogEntry
filterLogEntry ls (LogEntry s m) =
  if s `elem` ls then Just (LogEntry s m) else Nothing

-- | output everything
logAll :: [LogSeverity]
logAll = [minBound .. maxBound]

-- | output all but Diagnostic. Diagnostic is sometimes useful for debugging but can get noisy depending on how you use it.
nonDiagnostic :: [LogSeverity]
nonDiagnostic = List.tail logAll

-- | ordered list of all the prefixes added so far
type LoggerPrefix = [T.Text]

-- | The Logger effect
data Logger a r where
  Log :: a -> Logger a ()

-- | add one log entry of arbitrary type.  If you want to log with another type besides @LogEntry.
log :: FR.Member (Logger a) effs => a -> FR.Eff effs ()
log = FR.send . Log

-- | add one log-entry of the LogEntry type
logLE
  :: FR.Member (Logger LogEntry) effs => LogSeverity -> T.Text -> FR.Eff effs ()
logLE ls lm = log (LogEntry ls lm)

-- | Helper function for logging with monad-logger handler
logWithHandler
  :: Handler (FR.Eff effs) a -> FR.Eff (Logger a ': effs) x -> FR.Eff effs x
logWithHandler handler = FR.interpret (\(Log a) -> handler a)

-- | Prefix effect
data PrefixLog r where
  AddPrefix :: T.Text -> PrefixLog ()
  RemovePrefix :: PrefixLog ()
  GetPrefix :: PrefixLog T.Text

-- | Add one level of prefix
addPrefix :: FR.Member PrefixLog effs => T.Text -> FR.Eff effs ()
addPrefix = FR.send . AddPrefix

-- | remove last prefix
removePrefix :: FR.Member PrefixLog effs => FR.Eff effs ()
removePrefix = FR.send RemovePrefix

-- | get current prefix 
getPrefix :: FR.Member PrefixLog effs => FR.Eff effs T.Text
getPrefix = FR.send $ GetPrefix

-- | add a prefix for the block of code
wrapPrefix
  :: FR.Member PrefixLog effs => T.Text -> FR.Eff effs a -> FR.Eff effs a
wrapPrefix p l = do
  addPrefix p
  res <- l
  removePrefix
  return res

-- | interpret LogPrefix in State
prefixInState
  :: forall effs a
   . FR.Eff (PrefixLog ': effs) a
  -> FR.Eff (FR.State [T.Text] ': effs) a
prefixInState = FR.reinterpret $ \case
  AddPrefix t  -> FR.modify (\ps -> t : ps)
  RemovePrefix -> FR.modify @[T.Text] tail -- type application required here since tail is polymorphic
  GetPrefix    -> (FR.get >>= (return . T.intercalate "." . List.reverse))

-- | run the prefix effect by interpresting in State and running that
runPrefix :: FR.Eff (PrefixLog ': effs) a -> FR.Eff effs a
runPrefix = FR.evalState [] . prefixInState

-- | add a prefix to the log message and render
data WithPrefix a = WithPrefix { msgPrefix :: T.Text, discardPrefix :: a }
renderWithPrefix :: (a -> PP.Doc ann) -> WithPrefix a -> PP.Doc ann
renderWithPrefix k (WithPrefix pr a) = PP.pretty pr PP.<+> PP.align (k a)

-- | use prefix effect to map all the logged messages to WithPrefix form
logPrefixed
  :: FR.Member PrefixLog effs
  => FR.Eff (Logger a ': effs) x
  -> FR.Eff (Logger (WithPrefix a) ': effs) x
logPrefixed =
  FR.reinterpret (\(Log a) -> getPrefix >>= (\p -> log (WithPrefix p a)))

-- the use of "raise" below is there since we are running the handler in the stack that still has the LogPrefix effect.
-- I couldn't figure out how to write this the other way.
-- | Given a handler for @WithPrefix a@ in the remaining effects (IO, e.g.,), run the Logger and Prefix effects and handle all the logging
-- messages via that handler.
logAndHandlePrefixed
  :: forall effs a x
   . Handler (FR.Eff effs) (WithPrefix a)
  -> FR.Eff (Logger a ': (PrefixLog ': effs)) x
  -> FR.Eff effs x
logAndHandlePrefixed handler =
  runPrefix
    . logWithHandler (FR.raise . handler)
    . logPrefixed @(PrefixLog ': effs)

-- | Add a severity filter to a handler
filterLog
  :: Monad m
  => ([LogSeverity] -> a -> Bool)
  -> [LogSeverity]
  -> Handler m a
  -> Handler m a
filterLog filterF lss h a = if filterF lss a then h a else return ()

-- | Simplest handler, uses a function from message to Text and then outputs all messages in IO
logToIO :: MonadIO m => (a -> T.Text) -> Handler m a
logToIO toText = liftIO . T.putStrLn . toText

-- | preferred handler for @LogEntry@ type with prefixes
prefixedLogEntryToIO :: MonadIO m => Handler m (WithPrefix LogEntry)
prefixedLogEntryToIO = logToIO
  (PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . renderWithPrefix
    (ML.renderWithSeverity PP.pretty . logEntryToWithSeverity)
  )

-- | Use preferred handler and filter to output to IO
filteredLogEntriesToIO
  :: MonadIO (FR.Eff effs)
  => [LogSeverity]
  -> FR.Eff (Logger LogEntry ': (PrefixLog ': effs)) x
  -> FR.Eff effs x
filteredLogEntriesToIO lss = logAndHandlePrefixed
  (filterLog f lss $ prefixedLogEntryToIO)
  where f lss a = (severity $ discardPrefix a) `List.elem` lss

-- | constraint helper for a general log-message type
type PrefixedLogEffs a = '[PrefixLog, Logger a]

-- | constraint helper for @LogEntry@ type with prefixes
type LogWithPrefixes effs = FR.Members (PrefixedLogEffs LogEntry) effs

-- not sure how to use this in practice but we might need it if we call a function with a MonadLog constraint.
-- | instance to support using an exisitng function with a MonadLog constraint from a freer-simple stack. 
instance (ML.MonadLog a m, FR.LastMember m effs) => ML.MonadLog a (FR.Eff effs) where
  logMessageFree inj = FR.sendM $ ML.logMessageFree inj


