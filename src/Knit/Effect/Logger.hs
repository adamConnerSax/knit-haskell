{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
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
{-# LANGUAGE InstanceSigs    #-}
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

  -- * Subsets for filtering
  , logAll
  , nonDiagnostic

  -- * Constraints for convenience 
  , LogWithPrefixes
  , LogWithPrefixesLE

  -- * Re-Exports
  , Semantic
  , Member
  , Handler
  )
where

import qualified Polysemy                      as P
import           Polysemy                       ( Member
                                                , Semantic
                                                )
import           Polysemy.Internal              ( send )
import qualified Polysemy.State                as P

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


-- TODO: consider a more interesting Handler type.  As in co-log (https://hackage.haskell.org/package/co-log-core)
-- where you newtype it and then can exploit its profunctoriality.
-- I got lost here.  Was trying to be compatible with logging-effect but I'm not sure why
-- the idea that the runner takes a function to handle the logging in the rest of the stack seems okay.  Though why not be more direct
-- once we are using effects in the first place?  Isn't that handler a mix of pretty-printing and interpreting and we would
-- rather separate those concerns?  So we should have an (a -> Text) and then interpreters in whatever?  I guess we merge them because
-- conversion is uneccessary if we throw a message away?  But still, the interpreters could take the pretty-printers as arguments?
-- Parking this for now, since it has absorbed outsize time for no benefit except some understanding.

-- | Severity of message.  Based on monad-logger.
data LogSeverity = Diagnostic | Info | Warning | Error deriving (Show, Eq, Ord, Enum, Bounded)

-- | Map between @LogSeverity@ and monad-logger severity.
logSeverityToSeverity :: LogSeverity -> ML.Severity
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


-- | "LogSeverity" list used in order to output everything.
logAll :: [LogSeverity]
logAll = [minBound .. maxBound]

-- | 'LogSeverity' list used to output all but 'Diagnostic'.
-- 'Diagnostic' messages are sometimes useful for debugging but can get noisy depending on how you use it.
nonDiagnostic :: [LogSeverity]
nonDiagnostic = List.tail logAll

-- | The Logger effect
data Logger a m r where
  Log :: a -> Logger a m ()

-- | Add one log entry of arbitrary type.  If you want to log with another type besides @LogEntry.
log :: P.Member (Logger a) effs => a -> P.Semantic effs ()
log = send . Log

-- | Add one log-entry of the @LogEntry@ type.
logLE
  :: P.Member (Logger LogEntry) effs
  => LogSeverity
  -> T.Text
  -> P.Semantic effs ()
logLE ls lm = log (LogEntry ls lm)

-- | Helper function for logging with monad-logger handler.
logWithHandler
  :: Handler (P.Semantic effs) a
  -> P.Semantic (Logger a ': effs) x
  -> P.Semantic effs x
logWithHandler handler = P.interpret (\(Log a) -> handler a)

-- | Prefix Effect
data PrefixLog m r where
  AddPrefix :: T.Text -> PrefixLog m () -- ^ Represents adding a prefix to the logging output
  RemovePrefix :: PrefixLog m () -- ^ Represents removing one level of prefixing
  GetPrefix :: PrefixLog m T.Text -- ^ Represents retrieving the current prefix

-- | Add one level of prefix.
addPrefix :: P.Member PrefixLog effs => T.Text -> P.Semantic effs ()
addPrefix = send . AddPrefix

-- | Remove last prefix.
removePrefix :: P.Member PrefixLog effs => P.Semantic effs ()
removePrefix = send RemovePrefix

-- | Get current prefix 
getPrefix :: P.Member PrefixLog effs => P.Semantic effs T.Text
getPrefix = send $ GetPrefix

-- | Add a prefix for the block of code.
wrapPrefix
  :: P.Member PrefixLog effs => T.Text -> P.Semantic effs a -> P.Semantic effs a
wrapPrefix p l = do
  addPrefix p
  res <- l
  removePrefix
  return res

-- | Interpret LogPrefix in @Polysemy.State [T.Text]@.
prefixInState
  :: forall effs a
   . P.Semantic (PrefixLog ': effs) a
  -> P.Semantic (P.State [T.Text] ': effs) a
prefixInState = P.reinterpret $ \case
  AddPrefix t  -> P.modify (t :)
  RemovePrefix -> P.modify @[T.Text] tail -- type application required here since tail is polymorphic
  GetPrefix    -> fmap (T.intercalate "." . List.reverse) P.get

-- | Interpret the 'LogPrefix' effect in State and run that.
runPrefix :: P.Semantic (PrefixLog ': effs) a -> P.Semantic effs a
runPrefix = fmap snd . P.runState [] . prefixInState

-- | Monad-logger style wrapper to add prefixes to log messages.
data WithPrefix a = WithPrefix { msgPrefix :: T.Text, discardPrefix :: a }

-- | Render a prefixed log message with the pretty-printer.
renderWithPrefix :: (a -> PP.Doc ann) -> WithPrefix a -> PP.Doc ann
renderWithPrefix k (WithPrefix pr a) = PP.pretty pr PP.<+> PP.align (k a)

-- | Use @PrefixLog@ Effect to re-interpret all the logged messages to WithPrefix form.
logPrefixed
  :: P.Member PrefixLog effs
  => P.Semantic (Logger a ': effs) x
  -> P.Semantic (Logger (WithPrefix a) ': effs) x
logPrefixed =
  P.reinterpret (\(Log a) -> getPrefix >>= (\p -> log (WithPrefix p a)))

-- the use of "raise" below is there since we are running the handler in the stack that still has the LogPrefix effect.
-- I couldn't figure out how to write this the other way.
-- | Given a handler for @WithPrefix a@ in the remaining effects (IO, e.g.,), run the Logger and Prefix effects and handle all the logging
-- messages via that handler.
logAndHandlePrefixed
  :: forall effs a x
   . Handler (P.Semantic effs) (WithPrefix a)
  -> P.Semantic (Logger a ': (PrefixLog ': effs)) x
  -> P.Semantic effs x
logAndHandlePrefixed handler =
  runPrefix
    . logWithHandler (P.raise . handler)
    . logPrefixed @(PrefixLog ': effs)

-- | Add a severity filter to a handler.
filterLog
  :: Monad m
  => ([LogSeverity] -> a -> Bool)
  -> [LogSeverity]
  -> Handler m a
  -> Handler m a
filterLog filterF lss h a = when (filterF lss a) $ h a

-- | Simple handler, uses a function from message to Text and then outputs all messages in IO.
-- Can be used as base for any other handler that gives @Text@.
logToIO :: MonadIO m => (a -> T.Text) -> Handler m a
logToIO toText = liftIO . T.putStrLn . toText

-- | Preferred handler for @LogEntry@ type with prefixes.
prefixedLogEntryToIO :: MonadIO m => Handler m (WithPrefix LogEntry)
prefixedLogEntryToIO = logToIO
  (PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . renderWithPrefix
    (ML.renderWithSeverity PP.pretty . logEntryToWithSeverity)
  )

-- | Run the Logger and PrefixLog effects using the preferred handler and filter output in any Polysemy monad with IO in the union.
filteredLogEntriesToIO
  :: MonadIO (P.Semantic effs)
  => [LogSeverity]
  -> P.Semantic (Logger LogEntry ': (PrefixLog ': effs)) x
  -> P.Semantic effs x
filteredLogEntriesToIO lss = logAndHandlePrefixed
  (filterLog f lss $ prefixedLogEntryToIO)
  where f lss' a = (severity $ discardPrefix a) `List.elem` lss'

-- | Constraint helper for logging with prefixes
type LogWithPrefixes a effs = (P.Member PrefixLog effs, P.Member (Logger a) effs)

-- | Constraint helper for @LogEntry@ type with prefixes
type LogWithPrefixesLE effs = LogWithPrefixes LogEntry effs --(P.Member PrefixLog effs, P.Member (Logger a) effs)

{-
TODO: Working instance of logging-effect MonadLog.  Maybe.

• Illegal instance declaration for
        ‘ML.MonadLog a (P.Semantic effs)’
        The liberal coverage condition fails in class ‘ML.MonadLog’
          for functional dependency: ‘m -> message’
        Reason: lhs type ‘P.Semantic effs’ does not determine rhs type ‘a’
        Un-determined variable: a
    • In the instance declaration for ‘ML.MonadLog a (P.Semantic effs)’


-- not sure how to use this in practice but we might need it if we call a function with a MonadLog constraint.
-- | instance to support using an existing function with a MonadLog constraint from a freer-simple stack. 
instance (ML.MonadLog a m, P.Member (P.Lift m) effs) => ML.MonadLog a (P.Semantic effs) where
  logMessageFree :: (ML.MonadLog a m, P.Member (P.Lift m) effs,forall n. Monoid n => (a -> n) -> n) -> P.Semantic effs ()
  logMessageFree inj = P.sendM @m $ ML.logMessageFree inj

instance (P.Member (Logger a) effs) => ML.MonadLog a (P.Semantic effs) where
  logMessageFree inj = mapM_ log (inj $ pure @[])
-}
