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
module Control.Monad.Freer.PandocMonad
  ( Pandoc
  , PandocEffects
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

instance FR.Members '[Pandoc, FR.Error P.PandocError] effs => MonadError P.PandocError (FR.Eff effs) where
  throwError = FR.throwError
  catchError = FR.catchError

-- we handle logging within the existing effect system
pandocSeverity :: P.LogMessage -> Log.LogSeverity
pandocSeverity lm = case P.messageVerbosity lm of
  P.ERROR   -> Log.Error
  P.WARNING -> Log.Warning
  P.INFO    -> Log.Info

logPandocMessage
  :: FR.Member (Log.Logger Log.LogEntry) effs => P.LogMessage -> FR.Eff effs ()
logPandocMessage lm = FR.send $ Log.Log $ Log.LogEntry
  (pandocSeverity lm)
  (T.pack . P.showLogMessage $ lm)

type PandocEffects effs = (FR.Members '[Pandoc, FR.Error P.PandocError, Log.PrefixLog, (Log.Logger Log.LogEntry)] effs {-*, MonadError P.PandocError (FR.Eff effs)-})

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


