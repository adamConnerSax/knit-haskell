{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
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
module Control.Monad.Freer.PandocMonad
  (
    -- * Types
    Pandoc
  , PandocEffects
  -- * functions to run in IO
  , runPandoc
  , runPandocAndLoggingToIO
  -- * re-exports
  , P.PandocError
  )
where

import qualified Text.Pandoc                   as P
import qualified Text.Pandoc.MIME              as P
import qualified Control.Monad.Freer           as FR
import qualified Control.Monad.Freer.Error     as FR
import qualified Control.Monad.Freer.TH        as FR
import           Data.ByteString               as BS
import           Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import           Control.Monad.Except           ( MonadError(..) )
import qualified Control.Monad.Freer.Logger    as Log
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.LocalTime            ( TimeZone )
import           System.Random                  ( StdGen )

-- | Pandoc Effect
data Pandoc r where
  LookupEnv :: String -> Pandoc (Maybe String)
  GetCurrentTime :: Pandoc UTCTime
  GetCurrentTimeZone :: Pandoc TimeZone
  NewStdGen :: Pandoc StdGen
  NewUniqueHash :: Pandoc Int
  OpenURL :: String -> Pandoc (BS.ByteString, Maybe P.MimeType)
  ReadFileLazy :: FilePath -> Pandoc LBS.ByteString
  ReadFileStrict :: FilePath -> Pandoc BS.ByteString
  Glob :: String -> Pandoc [FilePath]
  FileExists :: FilePath -> Pandoc Bool
  GetDataFileName :: FilePath -> Pandoc FilePath
  GetModificationTime :: FilePath -> Pandoc UTCTime
  GetCommonState :: Pandoc P.CommonState
  PutCommonState :: P.CommonState -> Pandoc ()
  GetsCommonState :: (P.CommonState -> a) -> Pandoc a
  ModifyCommonState :: (P.CommonState -> P.CommonState) -> Pandoc ()
  Trace :: String -> Pandoc ()

FR.makeEffect ''Pandoc

-- | split off the error piece so we can handle directly
instance FR.Members '[Pandoc, FR.Error P.PandocError] effs => MonadError P.PandocError (FR.Eff effs) where
  throwError = FR.throwError
  catchError = FR.catchError

-- we handle logging within the existing effect system
-- | map pandoc severities to our logging system
pandocSeverity :: P.LogMessage -> Log.LogSeverity
pandocSeverity lm = case P.messageVerbosity lm of
  P.ERROR   -> Log.Error
  P.WARNING -> Log.Warning
  P.INFO    -> Log.Info

-- | handle the logging aspect with our logger
logPandocMessage
  :: FR.Member (Log.Logger Log.LogEntry) effs => P.LogMessage -> FR.Eff effs ()
logPandocMessage lm = FR.send $ Log.Log $ Log.LogEntry
  (pandocSeverity lm)
  (T.pack . P.showLogMessage $ lm)

-- * Constraint Helper
type PandocEffects effs = (FR.Members '[Pandoc, FR.Error P.PandocError, Log.PrefixLog, (Log.Logger Log.LogEntry)] effs {-*, MonadError P.PandocError (FR.Eff effs)-})

-- | PandocMonad instance so that pandoc functions can be run in the freer-simple stack
instance PandocEffects effs => P.PandocMonad (FR.Eff effs) where
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
  :: ( P.PandocMonad m
     , FR.LastMember m effs
     , FR.Member (Log.Logger Log.LogEntry) effs
     )
  => FR.Eff (Pandoc ': effs) a
  -> FR.Eff effs a
runPandoc = FR.interpretM
  (\case
    LookupEnv s            -> P.lookupEnv s
    GetCurrentTime         -> P.getCurrentTime
    GetCurrentTimeZone     -> P.getCurrentTimeZone
    NewStdGen              -> P.newStdGen
    NewUniqueHash          -> P.newUniqueHash
    OpenURL             s  -> P.openURL s
    ReadFileLazy        fp -> P.readFileLazy fp
    ReadFileStrict      fp -> P.readFileStrict fp
    Glob                fp -> P.glob fp
    FileExists          fp -> P.fileExists fp
    GetDataFileName     fp -> P.getDataFileName fp
    GetModificationTime fp -> P.getModificationTime fp
    GetCommonState         -> P.getCommonState
    PutCommonState    cs   -> P.putCommonState cs
    GetsCommonState   f    -> P.getsCommonState f
    ModifyCommonState f    -> P.modifyCommonState f
    Trace             s    -> P.trace s
  )

mergeEithers :: Either a (Either a b) -> Either a b
mergeEithers (Left  x        ) = Left x
mergeEithers (Right (Left  x)) = Left x
mergeEithers (Right (Right x)) = Right x

-- | run the Pandoc effects, and log messages with the given severity, over IO.  If there is a Pandoc error,
-- you will get a Left in the resulting Either.
runPandocAndLoggingToIO
  :: [Log.LogSeverity]
  -> FR.Eff
       '[Pandoc, (Log.Logger Log.LogEntry), Log.PrefixLog, FR.Error
         P.PandocError, P.PandocIO]
       a
  -> IO (Either P.PandocError a)
runPandocAndLoggingToIO lss =
  fmap mergeEithers
    . P.runIO
    . FR.runM
    . FR.runError
    . Log.filteredLogEntriesToIO lss
    . runPandoc


{-
runPandocAndLogViaLoggingT :: [Log.LogSeverity]
                        ->  FR.Eff '[Pandoc, (Log.Logger Log.LogEntry), Log.PrefixLog, FR.Error P.PandocError, P.PandocIO] a
                        -> IO (Either P.PandocError a)
runPandocAndLogViaLoggingT lss = fmap mergeEithers
                                 . P.runIO
                                 . FR.runM
                                 . FR.runError
                                 . (flip ML.runLoggingT handler . lift) . Log.logToMonadLogLE lss
                                 . runPandoc where
  handler = FR.raise . liftIO . print . Log.renderWithPrefix id

-}


