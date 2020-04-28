{-# LANGUAGE OverloadedStrings #-}
module Streamly.Cereal
  (
    encodeStreamly
  , decodeStreamly
  )
where

import qualified Streamly as Streamly
import qualified Streamly.Prelude as Streamly

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified Data.Word as Word

encodeStreamly :: (Cereal.Serialize a, Monad m) => a -> Streamly.SerialT m Word.Word8
encodeStreamly = encodePut . Cereal.put

encodePut :: Monad m => Cereal.Put -> Streamly.SerialT m Word.Word8
encodePut = Streamly.unfoldr BL.uncons . Cereal.runPutLazy

decodeStreamly :: (Cereal.Serialize a, Monad m) => Streamly.SerialT m Word.Word8 -> m (Either Text.Text a)
decodeStreamly = decodeGet Cereal.get

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

{-
decodeGet' :: Monad m => Cereal.Get a -> Streamly.SerialT m Word.Word8 -> m (Either Text.Text a)
decodeGet' g = Streamly.foldrM buildResult (return $ Cereal.Fail "Stream exhausted while decoding" BS.empty) where
  buildResult :: Word.Word8 -> m (Result a) -> m (Result a)
  buildResult nextByte mr = do
    r <- mr
    case r of
      Cereal.Partial f -> return (f 
      Cereal.Fail e _ -> return r
      Cereal.Done a _ -> return r
      Cereal.Partial f' -> return
-}
