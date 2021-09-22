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
#if MIN_VERSION_streamly(0,8,0)
#else
  , streamlyToKnitS
#endif
  , logStreamly
  )
where

import qualified Knit.Effect.Logger as Knit.Logger


#if MIN_VERSION_streamly(0,8,0)
#else
import qualified Streamly
import qualified Streamly.Internal.Prelude as Streamly
#endif
import qualified Polysemy

import qualified Control.Monad.Primitive as Prim

import           Control.Monad.Catch  (MonadThrow, MonadCatch)
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
newtype StreamlyM a = StreamlyM { unStreamlyM :: ReaderT StreamlyEffects IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader StreamlyEffects)
  deriving (MonadThrow, MonadCatch, MonadIO, Prim.PrimMonad, MonadBase IO, MonadBaseControl IO) via (ReaderT StreamlyEffects IO)

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

#if MIN_VERSION_streamly(0,8,0)
#else
{-# DEPRECATED streamlyToKnitS "This is mysteriously slow so will be removed.  Run all streams in the StreamlyM monad. Then lift using @streamlyToKnit@" #-}
-- | Serial streams work fine over Sem, so we can lift the effectful serial stream into @Sem r@ without running.
streamlyToKnitS :: (Polysemy.Member (Polysemy.Embed IO) r
                  , Knit.Logger.LogWithPrefixesLE r
                  )
                => Streamly.SerialT StreamlyM a -> Streamly.SerialT (Polysemy.Sem r) a
streamlyToKnitS = Streamly.hoist streamlyToKnit
{-# INLINEABLE streamlyToKnitS #-}
#endif
