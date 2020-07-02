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
  , serializeStreamlyViaList

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
import           Data.Int (Int64)
import qualified Data.Serialize                as S
import qualified Data.Text                     as T
import qualified Data.Word                     as Word

import qualified Streamly                      as Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Data.Fold                 as Streamly.Fold
import qualified Streamly.Memory.Array         as Streamly.Array
import qualified Streamly.Internal.Data.Array           as Streamly.Data.Array
import qualified Streamly.External.Cereal      as Streamly.Cereal

-- | Error Type for Serialization errors.  Simplifies catching and reporting them.
data SerializationError = SerializationError T.Text deriving (Show)


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
  { encodeOne :: forall a. c a => a -> ct
  , decodeOne :: forall a. c a => ct -> Either SerializationError a
  , encBytes :: ct -> Int64
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
serializeOne :: (c a, P.MemberWithError (P.Error SerializationError) r)
             => SerializeDict c ct
             -> Serialize SerializationError r a ct
serializeOne (SerializeDict encOne decOne bytes) =
  let enc a = return (encOne a, a)
      {-# INLINEABLE enc #-}      
      dec = P.fromEither @SerializationError . decOne
      {-# INLINEABLE dec #-}
  in Serialize enc dec bytes
{-# INLINEABLE serializeOne #-}

-- | Given a @'SerializeDict' c ct@ and @a@ satisfying @c [a]@--usually
-- true as long as @a@ satisfies @c a@--produce
-- the 'Serialize' record-of-functions to encode/decode @Streamly.SerialT (P.Sem r) a@,
-- by mapping the stream to a (lazy) list, and encoding that and
-- decoding as a list and creating the stream from that.
serializeStreamlyViaList ::
  (P.MemberWithError (P.Error SerializationError) r, P.Member (P.Embed IO) r, c [a])
  => SerializeDict c ct
  -> Serialize SerializationError r (Streamly.SerialT (P.Sem r) a) ct 
serializeStreamlyViaList (SerializeDict encOne decOne bytes) =
  let enc = Streamly.fold (Streamly.Fold.tee
                           (fmap encOne $ Streamly.Fold.toList) 
                           (fmap (Streamly.Data.Array.toStream) Streamly.Data.Array.write)
                          )
      {-# INLINEABLE enc #-}
      dec = P.fromEither . fmap Streamly.fromList . decOne
      {-# INLINEABLE dec #-}
  in Serialize enc dec bytes
{-# INLINEABLE serializeStreamlyViaList #-}


-- | type-alias for default in-memory storage type.
type DefaultCacheData = Streamly.Array.Array Word.Word8

-- | type-alias for default Serializer
type DefaultSerializer = S.Serialize

-- | Implementation of `SerializeDict` for the cereal serializer,
-- encoding to/decoding from `Streamly.Memory.Array.Array`
cerealStreamlyDict :: SerializeDict DefaultSerializer DefaultCacheData
cerealStreamlyDict =
  SerializeDict
  Streamly.Cereal.encodeStreamlyArray
  (mapLeft SerializationError .  Streamly.Cereal.decodeStreamlyArray)
  (fromIntegral . Streamly.Array.length)
{-# INLINEABLE cerealStreamlyDict #-}

-- | Map the left side of an Either
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right
{-# INLINEABLE mapLeft #-}
