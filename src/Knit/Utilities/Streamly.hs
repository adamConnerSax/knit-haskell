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
import qualified Knit.Effect.Environment as Knit.Environment

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

-- | Use the logging function in the Reader to log in a StreamlyM context.
logStreamly :: Knit.Logger.LogSeverity -> Text.Text -> StreamlyM ()
logStreamly ls t = do
  logFunction <- Reader.asks logIO
  Reader.liftIO $ logFunction ls t
{-# INLINEABLE logStreamly #-}

-- | IO with a ReaderTlayer we can use to expose effects we need.  For now just logging.
newtype StreamlyM a = StreamlyM { unStreamlyM :: Reader.ReaderT StreamlyEffects IO a }
  deriving newtype (Functor, Applicative, Monad, Reader.MonadReader StreamlyEffects)
  deriving (MonadThrow, MonadCatch, Reader.MonadIO, MonadBase IO, MonadBaseControl IO) via (Reader.ReaderT StreamlyEffects IO)

-- | lift a 'StreamlyM' computation into a 'Knit.Sem' computation
streamlyToKnit :: Knit.KnitEffects c k ct r => StreamlyM a -> Knit.Sem r a
streamlyToKnit sa = do
  curPrefix <- Knit.Logger.getPrefix
  logFunction <- Knit.Environment.getLogWithPrefixIO 
  let se = StreamlyEffects (\ls lmsg -> logFunction curPrefix (Knit.Logger.LogEntry ls lmsg))
  Polysemy.embed $ Reader.runReaderT (unStreamlyM sa) se
{-# INLINEABLE streamlyToKnit #-}

-- | Serial streams work fine over Sem, so we can lift the effectful serial stream into @Sem r@ without running.
streamlyToKnitS :: Knit.KnitEffects c k ct r => Streamly.SerialT StreamlyM a -> Streamly.SerialT (Knit.Sem r) a
streamlyToKnitS = Streamly.hoist streamlyToKnit
{-# INLINEABLE streamlyToKnitS #-}
