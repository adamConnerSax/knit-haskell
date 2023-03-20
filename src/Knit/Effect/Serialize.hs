{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Knit.Effect.Serialize
Description : Effect for serializing and deserializing data
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module provides the types and supporting functions to use various serializers
with the caching framework in knit-haskell.
A <https://hackage.haskell.org/package/cereal Cereal> implementation is
provided, but using a different implementation is straightforward.
See <https://github.com/adamConnerSax/knit-haskell/blob/master/examples/CacheExample2.hs CacheExample2>
for an example.

The cache framework expects an explicit dictionary (i.e., record-of-functions)
rather than a Typeclass for serialization.
Since various serializers use slightly different Typeclass structures and function names,
we can't write code which is polynorphic in the serialization type-class. But with an
explicit dictionary we get that flexibility.  Once a dictionary for encoding and decoding
objects of arbitrary type is provided, along with the typeclass constraint required to use it,
the code in this module can convert that into all the functions the caching effect
requires to cache that data type or streams of it.

This could be implemented as a more straightforward effect but at the cost of complicating
the use for streams.  Making the explicit dictionary available balances the flexibility of
being able to change serializers with the relative ease of bootstrapping the single item serializer
into a serializer for streams, etc.
-}
module Knit.Effect.Serialize
  (
    -- * Types
    SerializeDict(..)
  , Serialize(..)
  , SerializeEnv

    -- * Deploy Implementations
  , serializeOne
    -- * Implementations
  , DefaultCacheData
  , DefaultSerializer
  , cerealStreamlyDict

    -- * Reader for Serializer Dictionary
  , getSerializeDict
  , runSerializeEnv

    -- * Errors
  , SerializationError(..)
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import qualified Polysemy.Reader               as PR
import qualified Data.Serialize                as S
import qualified Data.Text                     as T
import qualified Data.Word                     as Word

import qualified Control.Exception as X
import qualified Control.Monad.IO.Class as MonadIO (MonadIO(liftIO))

#if MIN_VERSION_streamly(0,9,0)
import qualified Streamly.Data.Stream  as Streamly
import qualified Streamly.Data.Array   as Streamly.Array
#elif MIN_VERSION_streamly(0,8,0)
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Data.Array.Foreign   as Streamly.Array
#else
import qualified Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Memory.Array         as Streamly.Array
#endif
import qualified Streamly.External.ByteString  as Streamly.ByteString


import qualified Knit.Utilities.Streamly       as K
import qualified Knit.Effect.Logger            as KLog

-- | Error Type for Serialization errors.  Simplifies catching and reporting them.
newtype SerializationError = SerializationError T.Text deriving (Show)
instance X.Exception SerializationError

{- |
Encoding/decoding functions for Serializing data, made explicit
here so we can pass them around as part of a configuration.
Allows for different Serializers as well as
Serializing to different types of in-memory store.

NB: The first parameter is of kind @Type -> Constraint@, e.g., @S.Serialize@,
which must be satisfied by anything serializable by
the implementation.

This should be straightforward to write for any serializer, and is all that's required to use
a non-default serializer as long as it serializes to @ByteStream@
(or, less likely, @Streamly.Memory.Array.Array@)
-}

--data BytesOrDone bytes = Bytes !bytes | Done

data SerializeDict (c :: Type -> Constraint) (ct :: Type) where
  SerializeDict :: forall c ct bytes.
                   (forall a. c a => a -> bytes) -- ^ encode a
                -> (forall a. c a => bytes -> Either SerializationError a) -- ^ decode bytes into a
                -> (bytes -> ct) -- ^ turn builder into the type stored in the cache
                -> (ct -> bytes) -- ^ turn the cache type into bytes for deserialization
                -> (ct -> Int64) -- ^ size (in Bytes) of something of the cache type
                -> SerializeDict c ct

{-
parseAll :: (bytes -> Either SerializationError (a, BytesOrDone bytes))
         -> bytes
         -> Either SerializationError a
parseAll parseOne b = parseOne b >>= oneToAll where
  oneToAll (a, bOrD) = case bOrD of
    Done -> Right a
    Bytes _ -> Left $ SerializationError "Bytes remaining after decode in serializeOne.decode)"
-}

-- | Make the dictionary available within effect stacks
type SerializeEnv c ct = PR.Reader (SerializeDict c ct)

-- | access the dictionary
getSerializeDict :: P.Member (SerializeEnv c ct) r => P.Sem r (SerializeDict c ct)
getSerializeDict = PR.ask
{-# INLINEABLE getSerializeDict #-}

-- | run the SerializeEnv effect by giving it the dictionary for use by the cache functions
runSerializeEnv :: SerializeDict c ct -> P.Sem (SerializeEnv c ct : r) a -> P.Sem r a --P.InterpreterFor (SerializeEnv c ct) r
runSerializeEnv = PR.runReader
{-# INLINEABLE runSerializeEnv #-}

{- |
Record-of-functions type to carry encoding/decoding functions for Serializing data.
Allows for different Serializers as well as
Serializing to different types of in memory store.
@encode@ returns the encoded value *and* a (possibly buffered) copy of its input.
This is designed around serialization of streams, where the original (effectful) stream may be expensive to run. But once run,
we can return a "buffered" stream which just unfolds from a memory buffer.
In many cases, we will just return the input in that slot.
-}
data Serialize e r a ct where
  Serialize :: (P.Member (P.Error e) r)
            => (a -> P.Sem r (ct, a)) -- ^ Encode and provide a "cheap" copy (for things like streams)
            -> (ct -> P.Sem r a)      -- ^ Decode
            -> (ct -> Int64)          -- ^ Size (in Bytes)
            -> Serialize e r a ct

-- | Given a @'SerializeDict' c ct@ and @a@ satisfying @c a@,
-- produce the (trivial) 'Serialize' record-of-functions to encode/decode a single @a@.
serializeOne :: (c a
                , KLog.LogWithPrefixesLE r
                , P.Member (P.Error SerializationError) r)
             => SerializeDict c ct
             -> Serialize SerializationError r a ct
serializeOne (SerializeDict encode decode bytesToCT ctToBytes ctBytes) =
  let enc a = return (bytesToCT $ encode a, a)
      {-# INLINEABLE enc #-}
      dec x = KLog.wrapPrefix "serializeOne.dec" $ do
        KLog.khDebugLog "deserializing..."
        a <- P.fromEither @SerializationError $ decode $ ctToBytes x -- NB: should check for empty bs in return
        KLog.khDebugLog "deserializing complete."
        return a
      {-# INLINEABLE dec #-}
  in Serialize enc dec ctBytes
{-# INLINEABLE serializeOne #-}

{-
-- | Given a @'SerializeDict' c ct@ and @a@ satisfying @c a@, produce
-- the 'Serialize' record-of-functions to encode/decode @Streamly.SerialT (P.Sem r) a@,
-- by mapping the stream to a (lazy) list, and encoding that and
-- decoding as a list and creating the stream from that.
serializeStreamly ::
  (P.Member (P.Error SerializationError) r
  , KLog.LogWithPrefixesLE r
  , P.Member (P.Embed IO) r
  , c a
{-  , c Word.Word64-})
  => SerializeDict c ct
  -> Serialize SerializationError r (Streamly.SerialT K.StreamlyM a) ct
serializeStreamly sdict@(SerializeDict _ _ _ _ bytes) =
  let enc s =   KLog.wrapPrefix "serializeStreamly.encode" $ do
        KLog.logLE KLog.Diagnostic "Encoding and buffering..."
        let bufferF = fmap Streamly.Data.Array.toStream Streamly.Data.Array.write
        (encodedCT, buffered) <- K.streamlyToKnit $ Streamly.fold (Streamly.Fold.tee (streamlySerializeF sdict) bufferF) s
        KLog.logLE KLog.Diagnostic "Encoding and buffering complete."
        return (encodedCT, buffered)
      {-# INLINEABLE enc #-}
      dec arr = KLog.wrapPrefix "serializeStreamly.decode" $ streamlyDeserialize sdict arr
      {-# INLINEABLE dec #-}
  in Serialize enc dec bytes
{-# INLINEABLE serializeStreamly #-}

streamlySerializeF :: forall c m a ct.(Monad m, c a)
                   => SerializeDict c ct
                   -> Streamly.Fold.Fold m a ct
streamlySerializeF  (SerializeDict encodeOne _ bldrToCT _ _) =
  let fBuilder = Streamly.Fold.Fold step initial return where
        step !b !a = return $ b <> encodeOne a
        initial = return mempty
      toCT' bldr n = bldrToCT $ encodeOne (fromIntegral @Int @Word.Word64 n) <> bldr
  in toCT' <$> fBuilder <*> Streamly.Fold.length
{-# INLINEABLE streamlySerializeF #-}


streamlyDeserialize :: forall a c ct r. (KLog.LogWithPrefixesLE r
                                        , P.Member (P.Error SerializationError) r
                                        , c a
                                        )
                    => SerializeDict c ct
                    -> ct
                    -> P.Sem r (Streamly.SerialT K.StreamlyM a)
streamlyDeserialize (SerializeDict _ parseOne _ ctToBytes _) ct = do
  (l, bs) <- case parseOne @Word.Word64 (ctToBytes ct) of
    Left err -> P.throw err
    Right (l, bs) -> return (l, bs)
  KLog.logLE KLog.Diagnostic $ "creating deserialization stream for " <> show l <> " items."
  let --unfoldOne :: bytes -> K.StreamlyM (Maybe (a, bytes))
      unfoldOne x = do
        case x of
          Done -> return Nothing
          Bytes bytes -> case parseOne bytes of
            Left err -> MonadIO.liftIO $ X.throwIO err
            Right (a, bytes') -> return $ Just (a, bytes')
  return $ do
    Streamly.yieldM (K.logStreamly KLog.Diagnostic "deserializing stream")
    Streamly.unfoldrM unfoldOne bs
-}

#if MIN_VERSION_streamly(0,9,0)
handleEitherInStream :: Either SerializationError (Streamly.Stream K.StreamlyM a) -> Streamly.Stream K.StreamlyM a
handleEitherInStream e = case e of
    Left err -> Streamly.fromEffect $ MonadIO.liftIO $ X.throwIO err
    Right a -> Streamly.before (K.logStreamly (KLog.Debug 3) "Deserializing stream...") a

#else
handleEitherInStream :: Either SerializationError (Streamly.SerialT K.StreamlyM a) -> Streamly.SerialT K.StreamlyM a
handleEitherInStream e = do
#if MIN_VERSION_streamly(0,8,0)
  let fromEffect = Streamly.fromEffect
#else
  let fromEffect = Streamly.yieldM
#endif
  case e of
    Left err -> MonadIO.liftIO $ X.throwIO err
    Right a -> do
      fromEffect $ K.logStreamly (KLog.Debug 3) "Deserializing stream..."
      a
#endif

-- | type-alias for default in-memory storage type.
type DefaultCacheData = Streamly.Array.Array Word.Word8
type DefaultSerializer = CerealS
-- | type-alias for default Serializer
type CerealS = S.Serialize

-- | Implementation of `SerializeDict` for the cereal serializer,
-- encoding to/decoding from `Streamly.Memory.Array.Array`
cerealStreamlyDict :: SerializeDict CerealS DefaultCacheData
cerealStreamlyDict =
  SerializeDict
  S.encode
  (first (SerializationError . toText) . S.decode)
  Streamly.ByteString.toArray
  Streamly.ByteString.fromArray
  (fromIntegral . Streamly.Array.length)

{-
cerealStreamlyDict :: SerializeDict CerealS DefaultCacheData
cerealStreamlyDict =
  let bOrd bs = if BS.null bs then Done else Bytes bs
      strictPut !x = BSB.bytes $! S.runPut $ S.put x
      strictGet !bs = mapLeft (SerializationError . toText) $ second bOrd <$> S.runGetState S.get bs 0
  in SerializeDict
     strictPut
     strictGet
     (Streamly.ByteString.toArray . BSB.builderBytes) --  (Streamly.Cereal.byteStringToStreamlyArray . BL.toStrict . BB.toLazyByteString)
     Streamly.Cereal.streamlyArrayToByteString
     (fromIntegral . Streamly.Array.length)
{-# INLINEABLE cerealStreamlyDict #-}
-}

-- | Map the left side of an Either
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right
{-# INLINEABLE mapLeft #-}
