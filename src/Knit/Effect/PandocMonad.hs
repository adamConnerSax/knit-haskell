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
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.PandocMonad
Description : Polysemy PandocMonad effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Polysemy PandocMonad effect.
Allows a polysemy "stack" to satisfy a PandocMonad constraint.
This still needs to run on top of PandocIO
but that will likely be addressed at some point in the future,
just requiring IO at base and the Logging and Random effects.
-}
module Knit.Effect.PandocMonad
  (
    -- * Types
    Pandoc
  , PandocEffects
  , PandocEffectsIO

  -- * Actions
  , lookupEnv
  , getCurrentTime
  , getCurrentTimeZone
  , newStdGen
  , newUniqueHash
  , openURL
  , readFileLazy
  , readFileStrict
  , glob
  , fileExists
  , getDataFileName
  , getModificationTime
  , getCommonState
  , putCommonState
  , getsCommonState
  , modifyCommonState
  , logOutput
  , trace

  -- * Interpreters
  , interpretInPandocMonad
  , interpretInIO

  -- * Runners
  , runIO

  -- * Re-Exports
  , PA.PandocError
  )
where

import qualified Knit.Effect.Logger            as Log

import qualified Polysemy                      as P
--import qualified Polysemy.IO                   as P
import           Polysemy.Internal              ( send )
import           Polysemy.Internal.Combinators  ( stateful )
import qualified Polysemy.Error                as P
import qualified Text.Pandoc                   as PA
import qualified Text.Pandoc.MIME              as PA
import qualified Text.Pandoc.UTF8              as UTF8
--import qualified Text.Pandoc.Logging           as PA

import qualified Data.ByteString               as BS
import           Data.ByteString.Lazy          as LBS
import           Data.ByteString.Base64         ( decodeLenient )
import qualified Data.CaseInsensitive          as CI
import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError(..)
                                                , liftIO
                                                )
import qualified Network.URI                   as NU
import           Network.Socket                 ( withSocketsDo )
import qualified Network.HTTP.Client           as NHC
import qualified Network.HTTP.Client.TLS       as NHC
                                                ( tlsManagerSettings )
import qualified Network.HTTP.Client.Internal  as NHC
                                                ( addProxy )
import qualified Network.HTTP.Types.Header     as NH
                                                ( hContentType )

import qualified System.Environment            as IO
                                                ( lookupEnv
                                                , getEnv
                                                )
import qualified System.IO.Error               as IO
                                                ( tryIOError )
import qualified Data.Time                     as IO
                                                ( getCurrentTime )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.LocalTime            ( TimeZone )
import qualified Data.Time.LocalTime           as IO
                                                ( getCurrentTimeZone )
import           System.Random                  ( StdGen )
import qualified System.Random                 as IO
                                                ( newStdGen )
import           Data.Unique                    ( hashUnique )
import qualified Data.Unique                   as IO
                                                ( newUnique )
import qualified System.FilePath.Glob          as IO
                                                ( glob )
import qualified System.Directory              as IO
                                                ( getModificationTime )
import qualified System.Directory              as Directory
import qualified Debug.Trace
import qualified Control.Exception             as E

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
  LogOutput :: PA.LogMessage -> Pandoc m ()
  Trace :: String -> Pandoc m ()

P.makeSem ''Pandoc

-- TODO: Understand the error pieces better.  Some things are thrown in IO, not sure we catch those??
-- This is an orphan instance. Yuck.
-- | Split off the error piece. We will handle directly with the polysemy @Error@ effect
instance (P.Member (P.Error PA.PandocError) effs) => MonadError PA.PandocError (P.Sem effs) where
  throwError = P.throw
  catchError = P.catch

-- we handle logging within the existing effect system
-- | Map pandoc severities to our logging system.
pandocSeverity :: PA.LogMessage -> Log.LogSeverity
pandocSeverity lm = case PA.messageVerbosity lm of
  PA.ERROR   -> Log.Error
  PA.WARNING -> Log.Warning
  PA.INFO    -> Log.Info

-- | Handle the logging with the knit-haskell logging effect.
logPandocMessage
  :: P.Member (Log.Logger Log.LogEntry) effs => PA.LogMessage -> P.Sem effs ()
logPandocMessage lm = send $ Log.Log $ Log.LogEntry
  (pandocSeverity lm)
  (T.pack . PA.showLogMessage $ lm)

-- | Constraint helper for using this set of effects in IO.
type PandocEffects effs =
  ( P.Member Pandoc effs
  , P.Member (P.Error PA.PandocError) effs
  , P.Member Log.PrefixLog effs
  , P.Member (Log.Logger Log.LogEntry) effs)

-- This is an orphan instance. Yuck.
-- | PandocMonad instance so that pandoc functions can be run in the polysemy union effect
instance PandocEffects effs => PA.PandocMonad (P.Sem effs) where
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
  getsCommonState = getsCommonState
  modifyCommonState = modifyCommonState
  logOutput = logOutput --logPandocMessage
  trace = trace


-- | Constraint helper for using this set of effects in IO.
type PandocEffectsIO effs =
  ( PandocEffects effs
  , P.Member (P.Lift IO) effs)

-- | Interpret the Pandoc effect using @IO@, @Knit.Effect.Logger@ and @PolySemy.Error PandocError@ 
interpretInIO
  :: forall effs a
   . ( P.Member (Log.Logger Log.LogEntry) effs
     , P.Member (P.Lift IO) effs
     , P.Member (P.Error PA.PandocError) effs
     )
  => P.Sem (Pandoc ': effs) a
  -> P.Sem effs a
interpretInIO = fmap snd . stateful f PA.def
 where
  liftPair :: forall f x y . Functor f => (x, f y) -> f (x, y)
  liftPair (x, fy) = fmap (x, ) fy
  f :: Pandoc m x -> PA.CommonState -> P.Sem effs (PA.CommonState, x)
  f (LookupEnv s)         cs = liftPair (cs, liftIO $ IO.lookupEnv s)
  f GetCurrentTime        cs = liftPair (cs, liftIO $ IO.getCurrentTime)
  f GetCurrentTimeZone    cs = liftPair (cs, liftIO IO.getCurrentTimeZone)
  f NewStdGen             cs = liftPair (cs, liftIO IO.newStdGen)
  f NewUniqueHash         cs = liftPair (cs, hashUnique <$> liftIO IO.newUnique)
  f (OpenURL         url) cs = openURLWithState cs url
  f (ReadFileLazy    fp ) cs = liftPair (cs, liftIOError LBS.readFile fp)
  f (ReadFileStrict  fp ) cs = liftPair (cs, liftIOError BS.readFile fp)
  f (Glob            s  ) cs = liftPair (cs, liftIOError IO.glob s)
  f (FileExists fp) cs = liftPair (cs, liftIOError Directory.doesFileExist fp)
  f (GetDataFileName s  ) cs = liftPair (cs, liftIOError getDataFileName' s)
  f (GetModificationTime fp) cs =
    liftPair (cs, liftIOError IO.getModificationTime fp)
  f GetCommonState          cs = return (cs, cs)
  f (GetsCommonState   g  ) cs = return (cs, g cs)
  f (ModifyCommonState g  ) cs = return (g cs, ())
  f (PutCommonState    cs') _  = return (cs', ())
  f (LogOutput         msg) cs = liftPair (cs, logPandocMessage msg)
  f (Trace             msg) cs = liftPair
    ( cs
    , when (PA.stTrace cs) $ Debug.Trace.trace ("[trace]" ++ msg) (return ())
    )

-- | Interpret the Pandoc effect in another monad (which must satisy the PandocMonad constraint) and @Knit.Effect.Logger@
interpretInPandocMonad
  :: forall m effs a
   . ( PA.PandocMonad m
     , P.Member (P.Lift m) effs
     , P.Member (Log.Logger Log.LogEntry) effs
     )
  => P.Sem (Pandoc ': effs) a
  -> P.Sem effs a
interpretInPandocMonad = P.interpret
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
    LogOutput         msg  -> logPandocMessage msg
    Trace             s    -> P.sendM @m $ PA.trace s
  )

-- | Run the Pandoc effects,
-- and log messages with the given severity, over IO.
-- If there is a Pandoc error, you will get a Left in the resulting Either.
runIO
  :: [Log.LogSeverity]
  -> P.Sem
       '[Pandoc, Log.Logger Log.LogEntry, Log.PrefixLog, P.Error
         PA.PandocError, P.Lift IO]
       a
  -> IO (Either PA.PandocError a)
runIO lss =
  P.runM . P.runError . Log.filteredLogEntriesToIO lss . interpretInIO

-- copied from Pandoc code and modified as needed for Polysemy and my implementation of interpretInIO (PandocIO)
openURLWithState
  :: forall effs
   . ( P.Member (Log.Logger Log.LogEntry) effs
     , P.Member (P.Lift IO) effs
     , P.Member (P.Error PA.PandocError) effs
     )
  => PA.CommonState
  -> String
  -> P.Sem effs (PA.CommonState, (BS.ByteString, Maybe PA.MimeType))
openURLWithState cs u
  | Just u'' <- L.stripPrefix "data:" u = do
    let mime = L.takeWhile (/= ',') u''
    let contents = UTF8.fromString $ NU.unEscapeString $ L.drop 1 $ L.dropWhile
          (/= ',')
          u''
    return (cs, (decodeLenient contents, Just mime))
  | otherwise = do
    let toReqHeader (n, v) = (CI.mk (UTF8.fromString n), UTF8.fromString v)
        customHeaders = fmap toReqHeader $ PA.stRequestHeaders cs
    cs' <- report cs $ PA.Fetching u
    res <- liftIO $ E.try $ withSocketsDo $ do
      let parseReq = NHC.parseRequest
      proxy <- IO.tryIOError (IO.getEnv "http_proxy")
      let addProxy' x = case proxy of
            Left  _  -> return x
            Right pr -> parseReq pr
              >>= \r -> return (NHC.addProxy (NHC.host r) (NHC.port r) x)
      req <- parseReq u >>= addProxy'
      let req' = req
            { NHC.requestHeaders = customHeaders ++ NHC.requestHeaders req
            }
      resp <- NHC.newManager NHC.tlsManagerSettings >>= NHC.httpLbs req'
      return
        ( BS.concat $ LBS.toChunks $ NHC.responseBody resp
        , UTF8.toString `fmap` lookup NH.hContentType (NHC.responseHeaders resp)
        )
    case res of
      Right r -> return (cs', r)
      Left  e -> P.throw $ PA.PandocHttpError u e

-- | Stateful version of the Pandoc @report@ function, outputting relevant log messages
-- and adding them to the log kept in the state.
report
  :: (P.Member (Log.Logger Log.LogEntry) effs)
  => PA.CommonState
  -> PA.LogMessage
  -> P.Sem effs PA.CommonState
report cs msg = do
  let verbosity = PA.stVerbosity cs
      level     = PA.messageVerbosity msg
  when (level <= verbosity) $ logPandocMessage msg
  let stLog' = msg : (PA.stLog cs)
      cs'    = cs { PA.stLog = stLog' }
  return cs'

-- | Utility function to lift IO errors into Sem
liftIOError
  :: (P.Member (P.Error PA.PandocError) effs, P.Member (P.Lift IO) effs)
  => (String -> IO a)
  -> String
  -> P.Sem effs a
liftIOError f u = do
  res <- liftIO $ IO.tryIOError $ f u
  case res of
    Left  e -> P.throw $ PA.PandocIOError u e
    Right r -> return r

-- this default is built into Pandoc.  I could probably do something more useful here but maybe something depends on it??
-- or maybe the actual version on each machine has a correct local version??
-- TODO: Fix/Understand this
datadir :: FilePath
datadir
  = "/home/builder/hackage-server/build-cache/tmp-install/share/x86_64-linux-ghc-8.6.3/pandoc-2.7.2"

getDataFileName' :: FilePath -> IO FilePath
getDataFileName' fp = do
  dir <- E.catch @E.IOException (IO.getEnv "pandoc_datadir")
                                (\_ -> return datadir)
  return (dir ++ "/" ++ fp)
