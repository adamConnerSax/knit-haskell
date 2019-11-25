{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Knit.Report.Cache
Description : Combinators for using the AtomicCache effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module adds types, combinators and knit functions for using knit-haskell with the DataCache effect. 

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.
-}
module Knit.Report.Cache
  ( knitStore
  , knitRetrieve
  , knitRetrieveOrMake
  , knitClear
  )
where

import qualified Knit.Report.EffectStack       as K

import qualified Knit.Effect.AtomicCache       as C
import qualified Knit.Effect.Logger            as K

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Data.Serialize                as S

import           Text.Pandoc.Error              ( PandocError
                                                  ( PandocSomeError
                                                  , PandocIOError
                                                  )
                                                )
import qualified System.IO.Error               as IE

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

cerealStrict :: S.Serialize a => C.Serialize PandocError a BS.ByteString
cerealStrict = C.Serialize S.encode (mapLeft PandocSomeError . S.decode)

cerealLazy :: S.Serialize a => C.Serialize PandocError a BL.ByteString
cerealLazy = C.Serialize S.encodeLazy (mapLeft PandocSomeError . S.decodeLazy)

-- | Store an a (serialized to a strict ByteString) at key k. Throw PandocIOError on IOError.
knitStore :: (K.KnitEffects r, S.Serialize a) => T.Text -> a -> P.Sem r ()
knitStore k a = K.wrapPrefix "knitStore" $ do
  K.logLE K.Diagnostic $ "Called with k=" <> k
  P.mapError ioErrorToPandocError $ C.store cerealStrict k a

-- | Retrieve an a from the store at key k. Throw if not found or IOError
knitRetrieve :: (K.KnitEffects r, S.Serialize a) => T.Text -> P.Sem r a
knitRetrieve k = P.mapError ioErrorToPandocError $ C.retrieve cerealStrict k

-- | Retrieve an a from the store at key k.
-- If retrieve fails then perform the action and store the resulting a at key k. 
knitRetrieveOrMake
  :: forall a r
   . (K.KnitEffects r, S.Serialize a)
  => T.Text
  -> P.Sem r a
  -> P.Sem r a
knitRetrieveOrMake k toMake = K.wrapPrefix "knitRetrieveOrMake" $ do
  ma <- C.retrieveMaybe cerealStrict k
  case ma of
    Nothing -> do
      K.logLE K.Diagnostic $ k <> " not found in cache. Making..."
      a <- toMake
      knitStore k a
      K.logLE K.Diagnostic $ "Asset Stored."
      return a
    Just a -> do
      K.logLE K.Diagnostic $ k <> " found in cache."
      return a

-- | Clear the a stored at key k.
knitClear :: K.KnitEffects r => T.Text -> P.Sem r ()
knitClear k = P.mapError ioErrorToPandocError $ C.clear k


ioErrorToPandocError :: IE.IOError -> PandocError
ioErrorToPandocError e = PandocIOError (show e) e
