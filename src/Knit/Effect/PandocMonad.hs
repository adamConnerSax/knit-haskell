{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
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
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-|
Module      : Knit.Effect.PandocMonad
Description : Polysemy PandocMonad effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

Polysemy PandocMonad effect.
Allows a polysemy monad to handle
actions with a PandocMonad contraint via
polysemy effects and IO.
Has an "absorber" to convert functions with a @PandocMonad@ constraint. 
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

    -- * Pandoc <2.8 compatibility
  , textToPandocText
  , pandocTextToText
#if   MIN_VERSION_pandoc(2,8,0) 
  , absorbTemplateMonad
  , Template
  , interpretTemplateIO
#endif  
  
    -- * Interpreters
  , interpretInPandocMonad
  , interpretInIO

    -- * Runners
  , runIO

    -- * Interop
  , absorbPandocMonad

    -- * Re-Exports
  , PA.PandocError
  )
where

import Prelude hiding (takeWhile, dropWhile,drop)
import qualified Knit.Effect.Logger            as Log
import qualified Paths_knit_haskell            as Paths

import qualified Polysemy                      as P
import           Polysemy.Internal              ( send )
import           Polysemy.Internal.Combinators  ( stateful )
import qualified Polysemy.Error                as P
import qualified Polysemy.ConstraintAbsorber   as P
import qualified Polysemy.Reader               as P

import qualified Text.Pandoc                   as PA
import qualified Text.Pandoc.MIME              as PA
import qualified Text.Pandoc.UTF8              as UTF8


import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.ByteString.Base64         ( decodeLenient )
import qualified Data.CaseInsensitive          as CI

import qualified Data.Text                     as T
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError(..)
                                                , liftIO
                                                )

--import qualified Data.Constraint               as C

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

#if MIN_VERSION_pandoc(2,8,0)
import qualified Text.DocTemplates             as DT
import           Data.Functor.Identity          (runIdentity)
#else
import qualified Data.List as L
#endif

#if MIN_VERSION_pandoc(2,8,0)
type PandocText = T.Text

pandocTextToText :: PandocText -> T.Text
pandocTextToText = id

textToPandocText :: T.Text -> PandocText
textToPandocText = id

pandocTextToString :: PandocText -> String
pandocTextToString = T.unpack

stringToPandocText :: String -> PandocText
stringToPandocText = T.pack

pandocTextToBS :: PandocText -> BS.ByteString
pandocTextToBS = UTF8.fromText

stripPrefix :: T.Text -> T.Text -> Maybe T.Text 
stripPrefix = T.stripPrefix

takeWhile :: (Char -> Bool) -> T.Text -> T.Text
takeWhile = T.takeWhile

drop :: Int -> T.Text -> T.Text
drop = T.drop

dropWhile :: (Char -> Bool) -> T.Text -> T.Text
dropWhile = T.dropWhile

#else
type PandocText = String

pandocTextToText :: PandocText -> T.Text
pandocTextToText = T.pack

textToPandocText :: T.Text -> PandocText
textToPandocText = T.unpack

pandocTextToString :: PandocText -> String
pandocTextToString = id

stringToPandocText :: String -> PandocText
stringToPandocText = id

pandocTextToBS :: PandocText -> BS.ByteString
pandocTextToBS = UTF8.fromString

stripPrefix :: String -> String -> Maybe String 
stripPrefix = L.stripPrefix

takeWhile :: (Char -> Bool) -> String -> String
takeWhile = L.takeWhile

drop :: Int -> String -> String
drop = L.drop

dropWhile :: (Char -> Bool) -> String -> String
dropWhile = L.dropWhile

#endif

-- | Pandoc Effect
data Pandoc m r where
  LookupEnv :: PandocText -> Pandoc m (Maybe PandocText)
  GetCurrentTime :: Pandoc m UTCTime
  GetCurrentTimeZone :: Pandoc m TimeZone
  NewStdGen :: Pandoc m StdGen
  NewUniqueHash :: Pandoc m Int
  OpenURL :: PandocText -> Pandoc m (BS.ByteString, Maybe PA.MimeType)
  ReadFileLazy :: FilePath -> Pandoc m LBS.ByteString
  ReadFileStrict ::FilePath -> Pandoc m BS.ByteString
  Glob ::String -> Pandoc m [FilePath]
  FileExists :: FilePath -> Pandoc m Bool
  GetDataFileName :: FilePath -> Pandoc m FilePath
  GetModificationTime :: FilePath -> Pandoc m UTCTime
  GetCommonState :: Pandoc m PA.CommonState
  PutCommonState :: PA.CommonState -> Pandoc m ()
  GetsCommonState ::(PA.CommonState -> a) -> Pandoc m a
  ModifyCommonState ::(PA.CommonState -> PA.CommonState) -> Pandoc m  ()
  LogOutput ::PA.LogMessage -> Pandoc m ()
  Trace :: PandocText -> Pandoc m ()

P.makeSem ''Pandoc

#if MIN_VERSION_pandoc(2,8,0)
data Template m a where
  GetPartial :: FilePath -> Template m T.Text

P.makeSem ''Template

-- | Interpret a Template effect in any stack with IO (via the IO instance in DocTemplates)
interpretTemplateIO
  :: forall effs a
  . P.Member (P.Embed IO) effs
  => P.Sem (Template ': effs) a
  -> P.Sem effs a
interpretTemplateIO =  P.interpret $ \case
  GetPartial x -> P.embed $ DT.getPartial x

-- NB: This one ignores whatever getPartial is supposed to do but that's the only non-IO intepretation, I think?
-- | Interpret a Template effect in any stack (via the identity instance in DocTemplates)
interpretTemplatePure
  :: P.Sem (Template ': effs) a -> P.Sem effs a
interpretTemplatePure = P.interpret $ \case
  GetPartial x -> return . runIdentity $ DT.getPartial x

-- | use a Polysemy stack containing the 'Template` effect to run an TemplateMonad m action.
absorbTemplateMonad
  :: P.Member Template r => (DT.TemplateMonad (P.Sem r) => P.Sem r a) -> P.Sem r a
absorbTemplateMonad = P.absorbWithSem @DT.TemplateMonad @TemplateAction (TemplateDict getPartial) (P.Sub P.Dict)

-- | absorption gear for Template
newtype TemplateAction m s' a =
  TemplateAction { templateAction :: m a} deriving (Functor, Applicative, Monad)

data TemplateDict m = TemplateDict { getPartial_ :: FilePath -> m T.Text }

instance (Monad m, P.Reifies s' (TemplateDict m)) => DT.TemplateMonad (TemplateAction m s') where
  getPartial = TemplateAction . getPartial_ (P.reflect $ P.Proxy @s')

#endif
  
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
  (pandocTextToText . PA.showLogMessage $ lm)

-- | Constraint helper for using this set of effects in IO.
#if MIN_VERSION_pandoc(2,8,0)
type PandocEffects effs
  = ( P.Member Pandoc effs
    , P.Member Template effs
    , P.Member (P.Error PA.PandocError) effs
    , P.Member Log.PrefixLog effs
    , P.Member (Log.Logger Log.LogEntry) effs
    )
#else
type PandocEffects effs
  = ( P.Member Pandoc effs
    , P.Member (P.Error PA.PandocError) effs
    , P.Member Log.PrefixLog effs
    , P.Member (Log.Logger Log.LogEntry) effs
    )
#endif    
-- absorption gear
-- | absorb a @PandocMonad@ constraint into
--  @Members [Pandoc, Error PandocError] r => Sem r@
absorbPandocMonad
  :: P.Members '[P.Error PA.PandocError, Pandoc] r
  => (PA.PandocMonad (P.Sem r) => P.Sem r a)
  -> P.Sem r a
absorbPandocMonad = P.absorbWithSem @PA.PandocMonad @Action
  (PandocDict lookupEnv
              getCurrentTime
              getCurrentTimeZone
              newStdGen
              newUniqueHash
              openURL
              readFileLazy
              readFileStrict
              glob
              fileExists
              getDataFileName
              getModificationTime
              getCommonState
              putCommonState
              getsCommonState
              modifyCommonState
              logOutput
              trace
              P.throw
              P.catch
  )
  (P.Sub P.Dict)

-- | wrapper for the PandocMonad constrained action 
newtype Action m s' a = Action
    { action :: m a
    } deriving (Functor, Applicative, Monad)

-- | A dictionary of the functions we need to supply
-- to make an instance of PandocMonad
-- NB: the presence of @throwError@ and @catchError_@
-- which we need because of the MonadError superclass.
data PandocDict m = PandocDict
  {
    lookupEnv_ :: PandocText -> m (Maybe PandocText)
  , getCurrentTime_ :: m UTCTime
  , getCurrentTimeZone_ :: m TimeZone
  , newStdGen_ ::m StdGen
  , newUniqueHash_ :: m Int
  , openURL_ :: PandocText ->  m (BS.ByteString, Maybe PA.MimeType)
  , readFileLazy_ :: FilePath ->  m LBS.ByteString
  , readFileStrict_ :: FilePath ->  m BS.ByteString
  , glob_ :: String ->  m [FilePath]
  , fileExists_ :: FilePath ->  m Bool
  , getDataFileName_ :: FilePath ->  m FilePath
  , getModificationTime_ :: FilePath ->  m UTCTime
  , getCommonState_ :: m PA.CommonState
  , putCommonState_ :: PA.CommonState ->  m ()
  , getsCommonState_ :: forall a. (PA.CommonState -> a) ->  m a
  , modifyCommonState_ :: (PA.CommonState -> PA.CommonState) ->  m  ()
  , logOutput_ :: PA.LogMessage ->  m ()
  , trace_ :: PandocText ->  m ()
  , throwError_ :: forall a. PA.PandocError -> m a
  , catchError_ :: forall a. m a -> (PA.PandocError -> m a) -> m a
  }


instance (Monad m
         , P.Reifies s' (PandocDict m)) => MonadError PA.PandocError (Action m s') where
  throwError e = Action $ throwError_ (P.reflect $ P.Proxy @s') e
  catchError x f =
    Action $ catchError_ (P.reflect $ P.Proxy @s') (action x) (action . f)

instance (Monad m
         , MonadError PA.PandocError (Action m s')
         , P.Reifies s' (PandocDict m)) => PA.PandocMonad (Action m s') where
  lookupEnv           = Action . lookupEnv_ (P.reflect $ P.Proxy @s')
  getCurrentTime      = Action $ getCurrentTime_ (P.reflect $ P.Proxy @s')
  getCurrentTimeZone  = Action $ getCurrentTimeZone_ (P.reflect $ P.Proxy @s')
  newStdGen           = Action $ newStdGen_ (P.reflect $ P.Proxy @s')
  newUniqueHash       = Action $ newUniqueHash_ (P.reflect $ P.Proxy @s')
  openURL             = Action . openURL_ (P.reflect $ P.Proxy @s')
  readFileLazy        = Action . readFileLazy_ (P.reflect $ P.Proxy @s')
  readFileStrict      = Action . readFileStrict_ (P.reflect $ P.Proxy @s')
  glob                = Action . glob_ (P.reflect $ P.Proxy @s')
  fileExists          = Action . fileExists_ (P.reflect $ P.Proxy @s')
  getDataFileName     = Action . getDataFileName_ (P.reflect $ P.Proxy @s')
  getModificationTime = Action . getModificationTime_ (P.reflect $ P.Proxy @s')
  getCommonState      = Action $ getCommonState_ (P.reflect $ P.Proxy @s')
  putCommonState      = Action . putCommonState_ (P.reflect $ P.Proxy @s')
  getsCommonState     = Action . getsCommonState_ (P.reflect $ P.Proxy @s')
  modifyCommonState   = Action . modifyCommonState_ (P.reflect $ P.Proxy @s')
  logOutput           = Action . logOutput_ (P.reflect $ P.Proxy @s')
  trace               = Action . trace_ (P.reflect $ P.Proxy @s')

-- | Constraint helper for using this set of effects in IO.
type PandocEffectsIO effs = (PandocEffects effs, P.Member (P.Embed IO) effs)

-- | Interpret the Pandoc effect using @IO@, @Knit.Effect.Logger@ and @PolySemy.Error PandocError@ 
interpretInIO
  :: forall effs a
   . ( P.Member (Log.Logger Log.LogEntry) effs
     , P.Member (P.Embed IO) effs
     , P.Member (P.Error PA.PandocError) effs
     )
  => P.Sem (Pandoc ': effs) a
  -> P.Sem effs a
interpretInIO = fmap snd . stateful f PA.def
 where
  liftPair :: forall f x y . Functor f => (x, f y) -> f (x, y)
  liftPair (x, fy) = fmap (x, ) fy
  f :: Pandoc m x -> PA.CommonState -> P.Sem effs (PA.CommonState, x)
  f (LookupEnv s)         cs = liftPair (cs, liftIO $ fmap (fmap stringToPandocText) $ IO.lookupEnv (pandocTextToString s))
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
    , when (PA.stTrace cs) $ Debug.Trace.trace ("[trace]" ++ (pandocTextToString msg)) (return ())
    )


-- | Interpret the Pandoc effect in another monad (which must satisy the PandocMonad constraint) and @Knit.Effect.Logger@
interpretInPandocMonad
  :: forall m effs a
   . ( PA.PandocMonad m
     , P.Member (P.Embed m) effs
     , P.Member (Log.Logger Log.LogEntry) effs
     )
  => P.Sem (Pandoc ': effs) a
  -> P.Sem effs a
interpretInPandocMonad = P.interpret
  (\case
    LookupEnv s            -> P.embed @m $ PA.lookupEnv s
    GetCurrentTime         -> P.embed @m $ PA.getCurrentTime
    GetCurrentTimeZone     -> P.embed @m $ PA.getCurrentTimeZone
    NewStdGen              -> P.embed @m $ PA.newStdGen
    NewUniqueHash          -> P.embed @m $ PA.newUniqueHash
    OpenURL             s  -> P.embed @m $ PA.openURL s
    ReadFileLazy        fp -> P.embed @m $ PA.readFileLazy fp
    ReadFileStrict      fp -> P.embed @m $ PA.readFileStrict fp
    Glob                fp -> P.embed @m $ PA.glob fp
    FileExists          fp -> P.embed @m $ PA.fileExists fp
    GetDataFileName     fp -> P.embed @m $ PA.getDataFileName fp
    GetModificationTime fp -> P.embed @m $ PA.getModificationTime fp
    GetCommonState         -> P.embed @m $ PA.getCommonState
    PutCommonState    cs   -> P.embed @m $ PA.putCommonState cs
    GetsCommonState   f    -> P.embed @m $ PA.getsCommonState f
    ModifyCommonState f    -> P.embed @m $ PA.modifyCommonState f
    LogOutput         msg  -> logPandocMessage msg
    Trace             s    -> P.embed @m $ PA.trace s
  )

-- | Run the Pandoc effects,
-- and log messages with the given severity, over IO.
-- If there is a Pandoc error, you will get a Left in the resulting Either.
#if MIN_VERSION_pandoc(2,8,0)
runIO
  :: (Log.LogSeverity -> Bool)
  -> P.Sem
     '[Template
      , Pandoc
      , P.Reader Log.LogWithPrefixIO
      , Log.Logger Log.LogEntry
      , Log.PrefixLog
      , P.Error PA.PandocError
      , P.Embed IO]
     a
  -> IO (Either PA.PandocError a)
runIO logIf =
  P.runM . P.runError . Log.filteredLogEntriesToIO logIf . interpretInIO . interpretTemplateIO
#else
runIO
  :: (Log.LogSeverity -> Bool)
  -> P.Sem
     '[Pandoc
      , P.Reader Log.LogWithPrefixIO
      , Log.Logger Log.LogEntry
      , Log.PrefixLog
      , P.Error PA.PandocError
      , P.Embed IO]
     a
  -> IO (Either PA.PandocError a)
runIO logIf =
  P.runM . P.runError . Log.filteredLogEntriesToIO logIf . interpretInIO
#endif

-- copied from Pandoc code and modified as needed for Polysemy and my implementation of interpretInIO (PandocIO)
openURLWithState
  :: forall effs
   . ( P.Member (Log.Logger Log.LogEntry) effs
     , P.Member (P.Embed IO) effs
     , P.Member (P.Error PA.PandocError) effs
     )
  => PA.CommonState
  -> PandocText
  -> P.Sem effs (PA.CommonState, (BS.ByteString, Maybe PA.MimeType))
openURLWithState cs u
  | Just u'' <- stripPrefix "data:" u = do
    let mime = takeWhile (/= ',') u''
    let contents = UTF8.fromString $ (NU.unEscapeString . pandocTextToString) $ drop 1 $ dropWhile
          (/= ',')
          u''
    return (cs, (decodeLenient contents, Just mime))
  | otherwise = do
    let toReqHeader (n, v) = (CI.mk (pandocTextToBS n), pandocTextToBS v)
        customHeaders = fmap toReqHeader $ PA.stRequestHeaders cs
    cs' <- report cs $ PA.Fetching u
    res <- liftIO $ E.try $ withSocketsDo $ do
      let parseReq = NHC.parseRequest
      proxy <- IO.tryIOError (IO.getEnv "http_proxy")
      let addProxy' x = case proxy of
            Left  _  -> return x
            Right pr -> parseReq pr
              >>= \r -> return (NHC.addProxy (NHC.host r) (NHC.port r) x)
      req <- parseReq (pandocTextToString u) >>= addProxy'
      let req' = req
            { NHC.requestHeaders = customHeaders ++ NHC.requestHeaders req
            }
      resp <- NHC.newManager NHC.tlsManagerSettings >>= NHC.httpLbs req'
      return
        ( BS.concat $ LBS.toChunks $ NHC.responseBody resp
        , UTF8.toString `fmap` lookup NH.hContentType (NHC.responseHeaders resp)
        )
    case res of
      Right r -> return (cs', (\(x,y) -> (x, stringToPandocText <$> y))  r)
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
  :: (P.Member (P.Error PA.PandocError) effs, P.Member (P.Embed IO) effs)
  => (String -> IO a)
  -> String
  -> P.Sem effs a
liftIOError f u = do
  res <- liftIO $ IO.tryIOError $ f u
  case res of
    Left  e -> P.throw $ PA.PandocIOError (stringToPandocText u) e
    Right r -> return r

-- | adjust the directory the PandocMonad sees so that it will get
-- the right files when it falls back to default.  Knit-haskell installs
-- differently than pandoc does so that it can have its own templates as
-- well.
getDataFileName' :: FilePath -> IO FilePath
getDataFileName' fp = do
  dir <- E.catch @E.IOException (IO.getEnv "pandoc_datadir")
                                (\_ -> Paths.getDataDir)
  return (dir ++ "/pandoc-data/" ++ fp)
