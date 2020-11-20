{-# LANGUAGE BangPatterns        #-}
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
  , serializeStreamly

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
--import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Builder       as BB
import           Data.Int (Int64)
import qualified Data.Serialize                as S
import qualified Data.Text                     as T
import qualified Data.Word                     as Word

import qualified Control.Exception as X
import qualified Control.Monad.IO.Class as MonadIO (MonadIO(liftIO))

import qualified Streamly                      as Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Data.Fold            as Streamly.Fold
import qualified Streamly.Internal.Data.Fold   as Streamly.Fold
import qualified Streamly.Memory.Array         as Streamly.Array
import qualified Streamly.Internal.Data.Array  as Streamly.Data.Array
import qualified Streamly.External.Cereal      as Streamly.Cereal

import qualified Knit.Utilities.Streamly       as K
import qualified Knit.Effect.Logger            as KLog

-- | Error Type for Serialization errors.  Simplifies catching and reporting them.
data SerializationError = SerializationError T.Text deriving (Show)
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
data SerializeDict c ct =
  SerializeDict
  { encodeOne :: forall a. c a => a -> BB.Builder
  , decodeOne :: forall a. c a => BL.ByteString -> Either SerializationError a 
  , parseOne :: forall a. c a => BL.ByteString -> Either SerializationError (a, BL.ByteString)  
  , builderToCacheType :: BB.Builder -> ct
  , cacheTypeByteString :: ct -> BL.ByteString
  , cacheTypeBytes :: ct -> Int64
--  , encBytes :: BL.ByteString -> Int64
  }

-- | Make the dictionary available within effect stacks
type SerializeEnv c ct = PR.Reader (SerializeDict c ct)

-- | access the dictionary
getSerializeDict :: P.Member (SerializeEnv c ct) r => P.Sem r (SerializeDict c ct)
getSerializeDict = PR.ask
{-# INLINEABLE getSerializeDict #-}

-- | run the SerializeEnv effect by giving it the dictionary for use by the cache functions
runSerializeEnv :: SerializeDict c ct -> P.InterpreterFor (SerializeEnv c ct) r
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
  Serialize :: (P.MemberWithError (P.Error e) r)
            => (a -> P.Sem r (ct, a)) -- ^ Encode
            -> (ct -> P.Sem r a)      -- ^ Decode
            -> (ct -> Int64)          -- ^ Size (in Bytes)
            -> Serialize e r a ct

-- | Given a @'SerializeDict' c ct@ and @a@ satisfying @c a@,
-- produce the (trivial) 'Serialize' record-of-functions to encode/decode a single @a@.
serializeOne :: (c a
                , KLog.LogWithPrefixesLE r
                , P.MemberWithError (P.Error SerializationError) r)
             => SerializeDict c ct 
             -> Serialize SerializationError r a ct
serializeOne (SerializeDict encOne decOne _ builderToCT ctToByteString ctBytes) =
  let enc a = return (builderToCT $ encOne a, a)
      {-# INLINEABLE enc #-}      
      dec x = KLog.wrapPrefix "serializeOne.dec" $ do
        KLog.logLE KLog.Diagnostic "deserializing..."
        a <- P.fromEither @SerializationError $ decOne $ ctToByteString x -- NB: should check for empty bs in return 
        KLog.logLE KLog.Diagnostic "deserializing complete."
        return a
      {-# INLINEABLE dec #-}
  in Serialize enc dec ctBytes
{-# INLINEABLE serializeOne #-}


-- | Given a @'SerializeDict' c ct@ and @a@ satisfying @c [a]@--usually
-- true as long as @a@ satisfies @c a@--produce
-- the 'Serialize' record-of-functions to encode/decode @Streamly.SerialT (P.Sem r) a@,
-- by mapping the stream to a (lazy) list, and encoding that and
-- decoding as a list and creating the stream from that.
serializeStreamly ::
  (P.MemberWithError (P.Error SerializationError) r
  , KLog.LogWithPrefixesLE r
  , P.Member (P.Embed IO) r
  , c a
  , c Word.Word64)
  => SerializeDict c ct
  -> Serialize SerializationError r (Streamly.SerialT K.StreamlyM a) ct 
serializeStreamly sdict@(SerializeDict encOne _ parse builderToCT ctToByteString bytes) =
  let enc s =   KLog.wrapPrefix "serializeStreamly.encode" $ do
        KLog.logLE KLog.Diagnostic $ "Encoding and buffering..."
        let bufferF = fmap (Streamly.Data.Array.toStream) Streamly.Data.Array.write        
        ((l, encodedB), buffered) <- K.streamlyToKnit $ Streamly.fold (Streamly.Fold.tee (Streamly.Fold.tee Streamly.Fold.length (streamlySerializeF sdict)) bufferF) s
        KLog.logLE KLog.Diagnostic $ "Encoding and buffering complete."
        return (builderToCT (encOne (fromIntegral @_ @Word.Word64 l) <> encodedB), buffered)
      {-# INLINEABLE enc #-}
      dec arr = KLog.wrapPrefix "serializeStreamly.decode" $ do
        (l, bs) <- case parse @Word.Word64 (ctToByteString arr) of
          Left err -> P.throw err
          Right (l, bs) -> return (l, bs)        
        KLog.logLE KLog.Diagnostic $ "creating deserialization stream for " <> (T.pack $ show l) <> " items."
        return $ streamlyDeserialize sdict bs
      {-# INLINEABLE dec #-}
  in Serialize enc dec bytes
{-# INLINEABLE serializeStreamly #-}

streamlySerializeF :: forall m c a ct.(Monad m, c a) => SerializeDict c ct -> Streamly.Fold.Fold m a BB.Builder
streamlySerializeF sdict = Streamly.Fold.Fold step initial extract where
  step :: BB.Builder -> a -> m BB.Builder
  step b !a = return $ b <> (encodeOne sdict) a
  initial = return mempty
  extract = return . id

streamlyDeserialize :: forall a c ct.c a => SerializeDict c ct -> BL.ByteString -> Streamly.SerialT K.StreamlyM a
streamlyDeserialize sdict bs = do
  let unfoldOne :: BL.ByteString -> K.StreamlyM (Maybe (a, BL.ByteString))
      unfoldOne x = do
        if BL.null x
          then return Nothing
          else case (parseOne sdict) x of
                 Left err -> MonadIO.liftIO $ X.throwIO err
                 Right (a, bs') -> return $ Just (a, bs') 
  Streamly.yieldM (K.logStreamly KLog.Diagnostic "deserializing stream")
  Streamly.unfoldrM unfoldOne bs 

handleEitherInStream :: Either SerializationError (Streamly.SerialT K.StreamlyM a) -> Streamly.SerialT K.StreamlyM a
handleEitherInStream e = do
  case e of
    Left err -> MonadIO.liftIO $ X.throwIO err
    Right a -> do
      Streamly.yieldM $ K.logStreamly KLog.Diagnostic $ "Deserializing stream..."
      a

-- | type-alias for default in-memory storage type.
type DefaultCacheData = Streamly.Array.Array Word.Word8

-- | type-alias for default Serializer
type DefaultSerializer = S.Serialize

-- | Implementation of `SerializeDict` for the cereal serializer,
-- encoding to/decoding from `Streamly.Memory.Array.Array`
cerealStreamlyDict :: SerializeDict DefaultSerializer DefaultCacheData
cerealStreamlyDict =
  SerializeDict
  (S.execPut . S.put)
  (mapLeft (SerializationError . T.pack) . S.runGetLazy S.get)
  (\bs -> mapLeft (SerializationError . T.pack) $ S.runGetLazyState S.get bs)
  (Streamly.Cereal.lazyByteStringToStreamlyArray . BB.toLazyByteString)
  Streamly.Cereal.streamlyArrayToLazyByteString
  (fromIntegral . Streamly.Array.length)  
{-# INLINEABLE cerealStreamlyDict #-}

-- | Map the left side of an Either
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right
{-# INLINEABLE mapLeft #-}
