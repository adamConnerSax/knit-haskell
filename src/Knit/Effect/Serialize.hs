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


-}
module Knit.Effect.Serialize
  (
    -- * Effect
    Serialize(..)
  , encode
  , decode
  , encBytes
    -- * Interpretations
    -- * Errors
  , SerializationError(..)
  )
where

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import qualified Knit.Effect.Logger            as K

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor.Identity          (Identity(..))
import           Data.Int (Int64)
import qualified Data.Map                      as M
import qualified Data.Serialize                as S
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import qualified Data.Word                     as Word

import qualified Control.Exception             as Exception
import           Control.Monad                  ( join )
import qualified Control.Monad.Catch.Pure      as Exceptions

import qualified Streamly                      as Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Streamly.Internal.Prelude              as Streamly
import qualified Streamly.Data.Fold                 as Streamly.Fold
import qualified Streamly.Internal.Data.Fold                 as Streamly.Fold
import qualified Streamly.Memory.Array         as Streamly.Array
import qualified Streamly.Internal.Memory.Array         as Streamly.Array
import qualified Streamly.Internal.Data.Array           as Streamly.Data.Array
import qualified Streamly.External.Cereal      as Streamly.Cereal
import qualified Streamly.External.ByteString as Streamly.ByteString



import qualified System.IO                     as System
import qualified System.Directory              as System
import qualified System.IO.Error               as IO.Error


{- TODO:
1. Can this design be simplified, part 1. The Maybe in the TMVar seems like it should be uneccessary.
2. It'd be nice to make sure we can't leave the empty TMVar. Can this be done in a way so that it must be filled?
3. We should be able to factor out some things around handling the returned TMVar
-}
-- | Error Type for Serialization errors.  Simplifies catching and reporting them.
data SerializationError = SerializationError T.Text deriving (Show)


{- |
Effect for encoding/decoding functions for Serializing data.
Allows for different Serializers as well as
Serializing to different types of in memory store.
@encode@ returns the encoded value *and* a (possibly buffered) copy of its input. 
This is designed around serialization of streams, where the original (effectful) stream may be expensive to run. But once run,
we can return a "buffered" stream which just unfolds from a memory buffer.
In many cases, we will just return the input in that slot.

NB: First parameter is a constraint which muct be satisfied by anything serializable by
the implementation.
-}

data Serialize ct f m a where
  Encode :: f a -> Serialize ct m (ct, f a) 
  Decode :: ct -> Serialize ct m (f a)
  EncBytes :: ct -> Serialize ct m Int64

P.makeSem ''Serialize



-- | Use Pure CatchT to handle MonadCatch constraint
fixMonadCatch :: (P.MemberWithError (P.Error Exceptions.SomeException) r)
              => Streamly.SerialT (Exceptions.CatchT (P.Sem r)) a -> Streamly.SerialT (P.Sem r) a
fixMonadCatch = Streamly.hoist f where
  f :: forall r a. (P.MemberWithError (P.Error Exceptions.SomeException) r) =>  Exceptions.CatchT (P.Sem r) a -> P.Sem r a
  f = join . fmap P.fromEither . Exceptions.runCatchT
{-# INLINEABLE fixMonadCatch #-}

-- | Map the left side of an Either
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right
{-# INLINEABLE mapLeft #-}

-- | Encode/Decode functions for serializing to strict ByteStrings
runCerealStrictBS :: (P.MemberWithError (P.Error SerializationError) r
                     , S.Serialize a
                     )
  => P.InterpreterFor (Serialize BS.ByteString)
runCerealStrictBS =
  P.interpret $ \case
    Encode a    -> return $ (S.encode a, a)
    Decode ct   -> P.fromEither @SerializationError . mapLeft (SerializationError . T.pack) $ S.decode ct
    EncBytes ct -> fromIntegral . BS.length
{-# INLINEABLE runCerealStrictBS #-}

-- | Encode/Decode functions for serializing to lazy ByteStrings
runCerealLazyBS :: (P.MemberWithError (P.Error SerializationError) r
                   , S.Serialize a
                   )
  => P.InterpreterFor (Serialize BL.ByteString)
runCerealLazyBS =
  P.interpret $ \case
    Encode a    -> return $ (S.encodeLazy a, a)
    Decode ct   -> P.fromEither @SerializationError . mapLeft (SerializationError . T.pack) $ S.decodeLazy ct
    EncBytes ct -> BL.length
{-# INLINEABLE runCerealLazyBS #-}

-- | Encode/Decode functions for serializing to Streamly Streams
runCerealStreamly :: (S.Serialize a
                     , P.Member (P.Embed IO) r
                     , P.MemberWithError (P.Error SerializationError) r
                     )
                  => P.InterpreterFor (Serialize (Streamly.SerialT Identity Word.Word8))
runCerealStreamly =
  P.interpret $ \case
    Encode a -> return $ (Streamly.Cereal.encodeStreamly a, a)
    Decode ct -> P.fromEither . mapLeft C.DeSerializationError . runIdentity $ Streamly.Cereal.decodeStreamly ct
    EncBytes ct -> fromIntegral . runIdentity . Streamly.length ct
{-# INLINEABLE runCerealStreamly #-}

-- | Encode/Decode functions for serializing to Streamly Arrays
runCerealArray :: (S.Serialize a
                  , P.Member (P.Embed IO) r
                  , P.MemberWithError (P.Error C.CacheError) r
               ) => P.InterpreterFor (Serialize Streamly.Array.Array Word.Word8)
runCerealArray = C.Serialize
  (\a -> return $ (Streamly.Cereal.encodeStreamlyArray a, a))
  (P.fromEither . mapLeft C.DeSerializationError . Streamly.Cereal.decodeStreamlyArray)
  (fromIntegral . Streamly.Array.length)
{-# INLINEABLE runCerealArray #-}

-- | Encode/Decode functions for serializing Streamly streams to Streamly Arrays
-- When encoding, we also return a "buffered" stream so that the input stream is only "run" once.
cerealStreamArray :: forall r a.(S.Serialize a                                
                     , P.Member (P.Embed IO) r                
                     , P.MemberWithError (P.Error Exceptions.SomeException) r
                     , P.MemberWithError (P.Error C.CacheError) r
                     )
                  => C.Serialize r (Streamly.SerialT (P.Sem r) a) (Streamly.Array.Array Word.Word8)
cerealStreamArray = C.Serialize
  (Streamly.fold (Streamly.Fold.tee
                 (Streamly.Fold.lmapM (Streamly.Array.fromStream . Streamly.Cereal.encodeStreamly) $ Streamly.Fold.mconcat)
                 (fmap (Streamly.Data.Array.toStream) Streamly.Data.Array.write)
                 )
  )
  (return . fixMonadCatch . Streamly.Cereal.decodeStreamArray)
  (fromIntegral . Streamly.Array.length)
{-# INLINEABLE cerealStreamArray #-}

-- | Encode/Decode functions for serializing Streamly streams to Streamly Arrays, using the Cereal functions to encode/decode lists.
-- When encoding, we also return a "buffered" stream so that the input stream is only "run" once.
cerealStreamViaListArray :: (S.Serialize a
                            , P.Member (P.Embed IO) r                
                            , P.MemberWithError (P.Error Exceptions.SomeException) r
                            , P.MemberWithError (P.Error C.CacheError) r
                            )
                         => C.Serialize r (Streamly.SerialT (P.Sem r) a) (Streamly.Array.Array Word.Word8)
cerealStreamViaListArray = C.Serialize
  (Streamly.fold (Streamly.Fold.tee
                   (fmap ((Streamly.ByteString.toArray . S.runPut . S.putListOf S.put) ) $ Streamly.Fold.toList)
                   (fmap (Streamly.Data.Array.toStream) Streamly.Data.Array.write)
                 )
  )
  (P.fromEither . mapLeft (C.DeSerializationError . T.pack) . S.runGet (Streamly.Cereal.getStreamOf S.get) . Streamly.ByteString.fromArray)
  (fromIntegral . Streamly.Array.length)
{-# INLINEABLE cerealStreamViaListArray #-}
