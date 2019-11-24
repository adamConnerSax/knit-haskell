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
Module      : Knit.Report.DataCache
Description : Extra layer for running knit-haskell reports using the DataCache effect
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module adds types, combinators and knit functions for using knit-haskell with the DataCache effect. 

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.
-}
module Knit.Report.DataCache
  ( module Knit.Report
  )
where

import qualified Knit.Report                   as KR
import           Knit.Report             hiding ( KnitEffects
                                                , KnitOne
                                                , KnitMany
                                                , knitHtml
                                                , knitHtmls
                                                )
import qualified Knit.Effect.DataCache         as D

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Data.Serialize                as S

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import           Text.Pandoc                    ( PandocError(..) )

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

-- | Constraint alias for the effects we need (and run)
-- when calling Knit.
-- Anything inside a call to Knit can use any of these effects.
-- Any other effects will need to be run before @knitHtml(s)@
type KnitEffects r = (KR.KnitEffects r, P.Member (D.DataCache PandocError T.Text BL.ByteString) r)

-- | Constraint alias for the effects we need to knit one document
type KnitOne r = (KR.KnitOne r, P.Member (D.DataCache PandocError T.Text BL.ByteString) r)

-- | Constraint alias for the effects we need to knit multiple documents
type KnitMany r = (KR.KnitMany r, P.Member (D.DataCache PandocError T.Text BL.ByteString) r)

{-                
cerealStrict :: S.Serialize a => D.Serialize T.Text a BS.ByteString
cerealStrict = D.Serialize S.encode S.decode
-}

cerealLazy :: S.Serialize a => D.Serialize PandocError a BL.ByteString
cerealLazy = D.Serialize S.encodeLazy (mapLeft PandocSomeError . S.decodeLazy)

knitStore :: (KnitEffects r, S.Serialize a) => T.Text -> a -> P.Sem r ()
knitStore = D.store cerealLazy

knitRetrieve :: (KnitEffects r, S.Serialize a) => T.Text -> P.Sem r a
knitRetrieve = D.retrieve cerealLazy

knitRetrieveOrMake
  :: (KnitEffects r, S.Serialize a) => T.Text -> P.Sem r a -> P.Sem r a
knitRetrieveOrMake k toMake = do
  ma <- D.retrieveMaybe cerealLazy k
  case ma of
    Nothing -> toMake
    Just a  -> return a

knitClear :: KnitEffects r => T.Text -> P.Sem r ()
knitClear = D.clear


