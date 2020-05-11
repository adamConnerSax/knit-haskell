{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UndecidableInstances #-}
module Knit.Utilities.Streamly
  (
    StreamlyM
  , StreamlyEffects(..)
  , streamlyToKnit
  , streamlyToKnitS
  , logStreamly  
  )
where

import qualified Knit.Report as Knit
import qualified Knit.Effect.Logger as Knit.Logger

import qualified Streamly
import qualified Streamly.Internal.Prelude as Streamly

import qualified Polysemy 

--import qualified Polysemy.Error
--import           Control.Monad.Catch  (SomeException)

import qualified Control.Monad.Reader as Reader

import           Control.Monad.Catch  (MonadThrow, MonadCatch)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Text as Text

-- | record-of-functions to hold access to effects we want to have available in this
-- ReaderT over IO wrapper for Streamly
data StreamlyEffects = StreamlyEffects { logIO :: Knit.Logger.LogSeverity -> Text.Text -> IO () }

logStreamly :: Knit.Logger.LogSeverity -> Text.Text -> StreamlyM ()
logStreamly ls t = do
  logFunction <- Reader.asks logIO
  Reader.liftIO $ logFunction ls t

newtype StreamlyM a =
  StreamlyM {
  unStreamlyM :: Reader.ReaderT StreamlyEffects IO a
  }
  deriving newtype (Functor, Applicative, Monad, Reader.MonadReader StreamlyEffects)
  deriving (MonadThrow, MonadCatch, Reader.MonadIO, MonadBase IO, MonadBaseControl IO) via (Reader.ReaderT StreamlyEffects IO)


streamlyToKnit :: Knit.KnitEffects r => StreamlyM a -> Knit.Sem r a
streamlyToKnit sa = do
  curPrefix <- Knit.Logger.getPrefix
  logFunction <- Knit.Logger.monadIOLogger curPrefix
  let se = StreamlyEffects logFunction
  Polysemy.embed $ Reader.runReaderT (unStreamlyM sa) se


streamlyToKnitS :: Knit.KnitEffects r => Streamly.SerialT StreamlyM a -> Streamly.SerialT (Knit.Sem r) a
streamlyToKnitS = Streamly.hoist streamlyToKnit
