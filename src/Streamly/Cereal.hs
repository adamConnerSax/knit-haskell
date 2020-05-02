{-# LANGUAGE OverloadedStrings #-}
module Streamly.Cereal
  (
    encodeStreamly
  , decodeStreamly
  , encodeStream
  , decodeStream
  )
where

import qualified Streamly as Streamly
import qualified Streamly.Prelude as Streamly
import qualified Streamly.Internal.Prelude as Streamly (splitParse)
--import qualified Streamly.Internal.Data.Parser as Streamly.Parser
import qualified Streamly.Internal.Data.Parser.Types as Streamly.Parser

import qualified Control.Monad.Catch as Exceptions (MonadThrow(..), MonadCatch(..))

--import qualified Text.Pandoc as Pandoc

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity (Identity(..))
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified Data.Word as Word

-- These go through []. I'm hoping the list fuses away.
-- One issue is that Cereal wants a length first and that requires spine strictness
-- on the encoding side.
putStreamOf :: Cereal.Putter a -> Cereal.Putter (Streamly.SerialT Identity a)
putStreamOf pa = runIdentity . fmap (Cereal.putListOf pa) . Streamly.toList 
{-# INLINEABLE putStreamOf #-}

getStreamOf :: Monad m => Cereal.Get a -> Cereal.Get (Streamly.SerialT m a)
getStreamOf ga = fmap Streamly.fromList $ Cereal.getListOf ga
{-# INLINEABLE getStreamOf #-}

encodeStreamly :: (Cereal.Serialize a, Monad m) => a -> Streamly.SerialT m Word.Word8
encodeStreamly = encodePut . Cereal.put
{-# INLINEABLE encodeStreamly #-}

encodePut :: Monad m => Cereal.Put -> Streamly.SerialT m Word.Word8
encodePut = Streamly.unfoldr BL.uncons . Cereal.runPutLazy
{-# INLINEABLE encodePut #-}

decodeStreamly :: (Cereal.Serialize a, Monad m) => Streamly.SerialT m Word.Word8 -> m (Either Text.Text a)
decodeStreamly = decodeGet Cereal.get
{-# INLINEABLE decodeStreamly #-}

decodeGet :: Monad m => Cereal.Get a -> Streamly.SerialT m Word.Word8 -> m (Either Text.Text a)
decodeGet g s = go s $ Cereal.runGetPartial g where
  go x f = do
    y <- Streamly.uncons x 
    case y of
      Nothing -> return $ Left "Premature end of stream reached."
      Just (b, tx) -> case f (BS.singleton b) of
        Cereal.Fail e _ -> return $ Left $ "Cereal Error: " <> (Text.pack e)
        Cereal.Done a _ -> return $ Right a
        Cereal.Partial f' -> go tx f'
{-# INLINEABLE decodeGet #-}

encodeStream :: (Monad m, Cereal.Serialize a) => Streamly.SerialT m a -> Streamly.SerialT m Word.Word8
encodeStream = Streamly.concatMap encodeStreamly

-- NB this will keep decoding as until failure.  But it can't know why it failed so it
-- assumes failure indicates end of the input stream.
streamlyDecodeParser :: (Monad m, Exceptions.MonadThrow m, Cereal.Serialize a) => Streamly.Parser.Parser m Word.Word8 a
streamlyDecodeParser = Streamly.Parser.Parser step (return $ Cereal.runGetPartial Cereal.get) extract where
  step f w = case f $ BS.singleton w of
    Cereal.Fail e _ -> return $ Streamly.Parser.Error e
    Cereal.Done a _ -> return $ Streamly.Parser.Stop 0 a
    Cereal.Partial f' -> return $ Streamly.Parser.Skip 0 f' 
  extract _ = Exceptions.throwM $ Streamly.Parser.ParseError "Parsing error in streamlyDecodeParser (\"extract\" called)."   

decodeStream :: (Monad m, Exceptions.MonadCatch m, Cereal.Serialize a)
             => Streamly.SerialT m Word.Word8 -> Streamly.SerialT m a
decodeStream = Streamly.splitParse streamlyDecodeParser
