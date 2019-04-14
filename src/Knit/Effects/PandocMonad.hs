{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Control.Monad.Freer.Pandoc
Description : freer-simple logging effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

freer-simple PandocMonad effect.  Allows a freer-simple stack to satisfy a PandocMonad constraint.  This still needs to run on top of PandocIO
but that will likely be addressed at some point in the future, just requiring IO at base and the Logging and Random effects.
-}
module Knit.Effects.PandocMonad
  (
    -- * Types
    Pandoc
  , PandocEffects
  -- * functions to run in IO
  , runPandoc
  , runPandocAndLoggingToIO
  -- * re-exports
  , PA.PandocError
  )
where

import qualified Knit.Effects.Logger           as Log

import qualified Polysemy                      as P
import qualified Polysemy.IO                   as P
import           Polysemy.Internal              ( send )
import qualified Polysemy.Error                as P
import           Polysemy.Internal.Lift         (Lift(Lift,unLift))

{-
import qualified Control.Monad.Freer           as FR
import qualified Control.Monad.Freer.Error     as FR
import qualified Control.Monad.Freer.TH        as FR
-}

import qualified Text.Pandoc                   as PA
import qualified Text.Pandoc.MIME              as PA

import           Data.ByteString               as BS
import           Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.LocalTime            ( TimeZone )
import           System.Random                  ( StdGen )



-- | Pandoc Effect
data Pandoc m r where
  LookupEnv :: String -> Pandoc m (Maybe String)
  GetCurrentTime :: Pandoc m UTCTime
  GetCurrentTimeZone :: Pandoc m TimeZone
  NewStdGen :: Pandoc m StdGen
  NewUniqueHash :: Pandoc m Int
  OpenURL :: String -> Pandoc m (BS.ByteString, Maybe PA.MimeType)
  ReadFileLazy :: FilePath -> Pandoc m LBS.ByteString
  ReadFileStrict :: FilePath -> Pandoc m BS.ByteString
  Glob :: String -> Pandoc m [FilePath]
  FileExists :: FilePath -> Pandoc m Bool
  GetDataFileName :: FilePath -> Pandoc m FilePath
  GetModificationTime :: FilePath -> Pandoc m UTCTime
  GetCommonState :: Pandoc m PA.CommonState
  PutCommonState :: PA.CommonState -> Pandoc m ()
  GetsCommonState :: (PA.CommonState -> a) -> Pandoc m a
  ModifyCommonState :: (PA.CommonState -> PA.CommonState) -> Pandoc m  ()
  Trace :: String -> Pandoc m ()

P.makeSemantic ''Pandoc

-- | split off the error piece so we can handle directly
instance (P.Member Pandoc effs, P.Member (P.Error PA.PandocError) effs) => MonadError PA.PandocError (P.Semantic effs) where
  throwError = P.throw
  catchError = P.catch

-- we handle logging within the existing effect system
-- | map pandoc severities to our logging system
pandocSeverity :: PA.LogMessage -> Log.LogSeverity
pandocSeverity lm = case PA.messageVerbosity lm of
  PA.ERROR   -> Log.Error
  PA.WARNING -> Log.Warning
  PA.INFO    -> Log.Info

-- | handle the logging aspect with our logger
logPandocMessage
  :: P.Member (Log.Logger Log.LogEntry) effs
  => PA.LogMessage
  -> P.Semantic effs ()
logPandocMessage lm = send $ Log.Log $ Log.LogEntry
  (pandocSeverity lm)
  (T.pack . PA.showLogMessage $ lm)

-- * Constraint Helper
type PandocEffects effs =
  ( P.Member Pandoc effs
  , P.Member (P.Error PA.PandocError) effs
  , P.Member Log.PrefixLog effs
  , P.Member (Log.Logger Log.LogEntry) effs)

-- | PandocMonad instance so that pandoc functions can be run in the polysemy union effect
instance PandocEffects effs => PA.PandocMonad (P.Semantic effs) where
  lookupEnv = lookupEnv
  getCurrentTime = getCurrentTime
  getCurrentTimeZone = getCurrentTimeZone
  newStdGen = newStdGen
  newUniqueHash = newUniqueHash
  openURL = openURL
  readFileLazy = readFileLazy
  readFileStrict = readFileStrict
  glob = glob
  fileExists = fileExists
  getDataFileName = getDataFileName
  getModificationTime = getModificationTime
  getCommonState = getCommonState
  putCommonState = putCommonState
  modifyCommonState = modifyCommonState
  logOutput = logPandocMessage
  trace = trace

-- for now.  But we can split this up into IO
-- must run before logger, right?
-- | run the Pandoc effect in another monad which satisfies the PandocMonad constraint.  Someday this will become a MonadIO constraint.
runPandoc
  :: forall m effs a. (PA.PandocMonad m, P.Member (P.Lift m) effs)
--     , P.Member (Log.Logger Log.LogEntry) effs
  => P.Semantic (Pandoc ': effs) a
  -> P.Semantic effs a
runPandoc = P.interpret
  (\case
    LookupEnv s            -> P.sendM @m $ PA.lookupEnv s
    GetCurrentTime         -> P.sendM @m $ PA.getCurrentTime
    GetCurrentTimeZone     -> P.sendM @m $ PA.getCurrentTimeZone
    NewStdGen              -> P.sendM @m $ PA.newStdGen
    NewUniqueHash          -> P.sendM @m $ PA.newUniqueHash
    OpenURL             s  -> P.sendM @m $ PA.openURL s
    ReadFileLazy        fp -> P.sendM @m $ PA.readFileLazy fp
    ReadFileStrict      fp -> P.sendM @m $ PA.readFileStrict fp
    Glob                fp -> P.sendM @m $ PA.glob fp
    FileExists          fp -> P.sendM @m $ PA.fileExists fp
    GetDataFileName     fp -> P.sendM @m $ PA.getDataFileName fp
    GetModificationTime fp -> P.sendM @m $ PA.getModificationTime fp
    GetCommonState         -> P.sendM @m $ PA.getCommonState
    PutCommonState    cs   -> P.sendM @m $ PA.putCommonState cs
    GetsCommonState   f    -> P.sendM @m $ PA.getsCommonState f
    ModifyCommonState f    -> P.sendM @m $ PA.modifyCommonState f
    Trace             s    -> P.sendM @m $ PA.trace s
  )

mergeEithers :: Either a (Either a b) -> Either a b
mergeEithers (Left  x        ) = Left x
mergeEithers (Right (Left  x)) = Left x
mergeEithers (Right (Right x)) = Right x

{-
subsumed by Polysemy.IO.runIO

-- I need a proof that (MonadIO m, P.Member (P.Lift m) r) :~: MonadIO (P.Semantic r) 
-- | Create a (Lift IO) in the stack for other effects that need it.  Is this the right way to do this? 
runIOInPandocIO :: P.Member (P.Lift PA.PandocIO) effs => P.Semantic (P.Lift IO ': effs) x -> P.Semantic effs x
runIOInPandocIO = P.interpret ((P.sendM @PA.PandocIO) . liftIO . unLift)
-}

-- | run the Pandoc effects, and log messages with the given severity, over IO.  If there is a Pandoc error,
-- you will get a Left in the resulting Either.
runPandocAndLoggingToIO
  :: [Log.LogSeverity]
  -> P.Semantic
       '[Pandoc, Log.Logger Log.LogEntry, Log.PrefixLog, P.Error
         PA.PandocError, P.Lift IO, P.Lift PA.PandocIO]
       a
  -> IO (Either PA.PandocError a)
runPandocAndLoggingToIO lss =
  fmap mergeEithers
    . PA.runIO    
    . P.runM
    . P.runIO @PA.PandocIO --use PandocIO to interpret (Lift IO), basically.
    . P.runError
    . Log.filteredLogEntriesToIO lss
    . runPandoc @PA.PandocIO


{-
runPandocAndLogViaLoggingT :: [Log.LogSeverity]
                        ->  FR.Eff '[Pandoc, (Log.Logger Log.LogEntry), Log.PrefixLog, FR.Error PA.PandocError, PA.PandocIO] a
                        -> IO (Either PA.PandocError a)
runPandocAndLogViaLoggingT lss = fmap mergeEithers
                                 . PA.runIO
                                 . FR.runM
                                 . FR.runError
                                 . (flip ML.runLoggingT handler . lift) . Log.logToMonadLogLE lss
                                 . runPandoc where
  handler = FR.raise . liftIO . print . Log.renderWithPrefix id

-}


