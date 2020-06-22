{-# LANGUAGE OverloadedStrings #-}
module Streamly.External.Cereal
  (
    encodeStreamly
  , decodeStreamly
  , encodeStreamlyArray
  , decodeStreamlyArray  
  , encodeStream
  , decodeStream
  , encodeStreamArray
  , decodeStreamArray
  , putStreamOf
  , getStreamOf
  )
where

import qualified Streamly as Streamly
import qualified Streamly.Prelude as Streamly
import qualified Streamly.Internal.Prelude as Streamly (splitParse)
import qualified Streamly.Internal.Memory.Array as Streamly.Array
import qualified Streamly.Internal.Memory.ArrayStream as Streamly.Array
import qualified Streamly.Internal.Data.Parser.Types as Streamly.Parser
import qualified Streamly.External.ByteString as Streamly.ByteString

import qualified Control.Monad.Catch as Exceptions (MonadThrow(..), MonadCatch(..))
import Control.Monad.IO.Class (MonadIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity (Identity(..))
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified Data.Word as Word

-- These go through []. I'm hoping the list fuses away.
-- One issue is that Cereal wants a length first and that requires spine strictness
-- on the encoding side.
-- | Convert something which can encode an @a@ to something which can encode a (non-effectful) stream of @a@
putStreamOf :: Cereal.Putter a -> Cereal.Putter (Streamly.SerialT Identity a)
putStreamOf pa = runIdentity . fmap (Cereal.putListOf pa) . Streamly.toList 
{-# INLINEABLE putStreamOf #-}

-- | Convert something which can decode an @a@ to something which can decode a (possibly-effectful) stream of @a@
getStreamOf :: Monad m => Cereal.Get a -> Cereal.Get (Streamly.SerialT m a)
getStreamOf ga = fmap Streamly.fromList $ Cereal.getListOf ga
{-# INLINEABLE getStreamOf #-}

-- | Given @Serialize a@, encode to a Stream of bytes 
encodeStreamly :: (Cereal.Serialize a, Monad m) => a -> Streamly.SerialT m Word.Word8
encodeStreamly = encodePut . Cereal.put
{-# INLINEABLE encodeStreamly #-}

-- | Given @Serialize a@, encode to n array of bytes
encodeStreamlyArray :: Cereal.Serialize a => a -> Streamly.Array.Array Word.Word8
encodeStreamlyArray = encodePutArray . Cereal.put
{-# INLINEABLE encodeStreamlyArray #-}

-- | Given a Cereal "Putter", encode to a byte stream
encodePut :: Monad m => Cereal.Put -> Streamly.SerialT m Word.Word8
encodePut = Streamly.unfoldr BL.uncons . Cereal.runPutLazy
{-# INLINEABLE encodePut #-}

-- | Given a Cereal "Putter", encode to a byte array
encodePutArray :: Cereal.Put -> Streamly.Array.Array Word.Word8
encodePutArray = Streamly.ByteString.toArray  . Cereal.runPut
{-# INLINEABLE encodePutArray #-}

-- | Give an instance of `@Serialize` for @a@, attempt to decode a byte-stream into an a.
decodeStreamly :: (Cereal.Serialize a, Monad m) => Streamly.SerialT m Word.Word8 -> m (Either Text.Text a)
decodeStreamly = decodeGet Cereal.get
{-# INLINEABLE decodeStreamly #-}

-- | Given a Cereal @Getter@, decode one @a@ from an effectful stream on bytes. 
decodeGet :: Monad m => Cereal.Get a -> Streamly.SerialT m Word.Word8 -> m (Either Text.Text a)
decodeGet g s = go s $ Cereal.runGetPartial g where
  go x f = do
    y <- Streamly.uncons x 
    case y of
      Nothing -> return $ Left "Premature end of stream reached."
      Just (b, tx) -> case f $ BS.singleton b of
        Cereal.Fail e _ -> return $ Left $ "Cereal Error: " <> (Text.pack e)
        Cereal.Done a _ -> return $ Right a
        Cereal.Partial f' -> go tx f'
{-# INLINEABLE decodeGet #-}

-- | Given @Serialize a@, attempt to decode a Streamly array of bytes into an @a@
decodeStreamlyArray :: (Cereal.Serialize a) => Streamly.Array.Array Word.Word8 -> Either Text.Text a
decodeStreamlyArray = either (Left . Text.pack) Right . Cereal.decode . Streamly.ByteString.fromArray 
{-# INLINEABLE decodeStreamlyArray #-}

-- | Given @Serialize a@, attempt to encode a stream of @a@s as a Streamly stream of bytes.
encodeStream :: (Monad m, Cereal.Serialize a) => Streamly.SerialT m a -> Streamly.SerialT m Word.Word8
encodeStream = Streamly.concatMap encodeStreamly
{-# INLINEABLE encodeStream #-}

-- | Given @Serialize a@, attempt to encode a sterm of @a@s as a Streamly array of bytes.
encodeStreamArray :: (Monad m, MonadIO m, Cereal.Serialize a) => Streamly.SerialT m a -> m (Streamly.Array.Array Word.Word8)
encodeStreamArray = Streamly.Array.toArray . Streamly.map encodeStreamlyArray 
{-# INLINEABLE encodeStreamArray #-}

-- NB this will keep decoding as until failure.  But it can't know why it failed so it
-- assumes failure indicates end of the input stream.
-- Parser state is (Maybe a, ByteStream -> Cereal.Result a)
-- | Streamly Parser for decoding bytes into @a@s (given @Serialize a@) 
streamlyDecodeParser :: (Monad m, Exceptions.MonadThrow m, Cereal.Serialize a) => Streamly.Parser.Parser m Word.Word8 a
streamlyDecodeParser = Streamly.Parser.Parser step (return $ (Nothing, Cereal.runGetPartial Cereal.get)) extract where
  step (_, f) w = case f $ BS.singleton w of
    Cereal.Fail e _ -> return $ Streamly.Parser.Error e
    Cereal.Done a _ -> return $ Streamly.Parser.Yield 0 (Just a, Cereal.runGetPartial Cereal.get)
    Cereal.Partial f' -> return $ Streamly.Parser.Skip 0 (Nothing, f')
  extract (ma, _)  = case ma of
    Just a -> return a
    Nothing -> Exceptions.throwM $ Streamly.Parser.ParseError "Parsing error in streamlyDecodeParser (\"extract\" called on incomplete parse.)."   
{-# INLINEABLE streamlyDecodeParser #-}

-- | Given @Serialize a@, decode a Stream of bytes into a stream of @a@s
decodeStream :: (Monad m, Exceptions.MonadCatch m, Cereal.Serialize a)
             => Streamly.SerialT m Word.Word8 -> Streamly.SerialT m a
decodeStream = Streamly.splitParse streamlyDecodeParser
{-# INLINEABLE decodeStream #-}

-- we convert the array to a stream so we can decode lazily (?)
-- | Given @Serialize a@, decode an array of bytes into a stream of @a@s
decodeStreamArray :: (Monad m, Exceptions.MonadCatch m, Cereal.Serialize a)
                  => Streamly.Array.Array Word.Word8 -> Streamly.SerialT m a
decodeStreamArray = decodeStream . Streamly.Array.toStream 
{-# INLINEABLE decodeStreamArray #-}

