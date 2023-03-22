{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnboxedTuples              #-} -- This is required for the PrimMonad instance
{-# LANGUAGE UndecidableInstances       #-}

module Knit.Utilities.Streamly
  (
    StreamlyM
  , StreamlyEffects(..)
  , streamlyToKnit
  , logStreamly
  )
where

import qualified Knit.Effect.Logger as Knit.Logger

import qualified Polysemy

import           Control.Monad.Catch  (MonadThrow, MonadCatch)
import qualified Control.Monad.Primitive as Prim
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Text as Text

-- | record-of-functions to hold access to effects we want to have available in this
-- ReaderT over IO wrapper for Streamly
newtype StreamlyEffects = StreamlyEffects { logIO :: Knit.Logger.LogSeverity -> Text.Text -> IO () }

-- | Use the logging function in the Reader to log in a StreamlyM context.
logStreamly :: Knit.Logger.LogSeverity -> Text.Text -> StreamlyM ()
logStreamly ls t = do
  logFunction <- asks logIO
  liftIO $ logFunction ls t
{-# INLINEABLE logStreamly #-}


-- | IO with a ReaderT layer we can use to expose effects we need.  For now just logging.
#if MIN_VERSION_streamly(0,9,0)
newtype StreamlyM a = StreamlyM { unStreamlyM :: ReaderT StreamlyEffects IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader StreamlyEffects)
  deriving (MonadThrow, MonadCatch, MonadIO, Prim.PrimMonad, MonadBase IO, MonadBaseControl IO) via (ReaderT StreamlyEffects IO)
#else
newtype StreamlyM a = StreamlyM { unStreamlyM :: ReaderT StreamlyEffects IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader StreamlyEffects)
  deriving (MonadThrow, MonadCatch, MonadIO, Prim.PrimMonad, MonadBase IO, MonadBaseControl IO) via (ReaderT StreamlyEffects IO)
#endif

-- | lift a 'StreamlyM' computation into a 'Knit.Sem' computation
streamlyToKnit :: (Polysemy.Member (Polysemy.Embed IO) r
                  , Knit.Logger.LogWithPrefixesLE r
                  )
  => StreamlyM a -> Polysemy.Sem r a
streamlyToKnit sa = do
  curPrefix <- Knit.Logger.getPrefix
  let logFunction = Knit.Logger.logWithPrefixToIO
      se = StreamlyEffects (\ls lmsg -> logFunction curPrefix (Knit.Logger.LogEntry ls lmsg))
  Polysemy.embed $ runReaderT (unStreamlyM sa) se
{-# INLINEABLE streamlyToKnit #-}
