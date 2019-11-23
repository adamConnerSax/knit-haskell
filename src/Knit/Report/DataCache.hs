{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ConstraintKinds      #-}
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


-- | Constraint alias for the effects we need (and run)
-- when calling Knit.
-- Anything inside a call to Knit can use any of these effects.
-- Any other effects will need to be run before @knitHtml(s)@
type KnitEffects r = (KR.KnitEffects r, K.Member (D.DataCache P.PandocError T.Text BL.ByteString) r)

-- | Constraint alias for the effects we need to knit one document
type KnitOne r = (KR.KnitOne r, P.Member (D.DataCache P.PandocError T.Text BL.ByteString) r)

-- | Constraint alias for the effects we need to knit multiple documents
type KnitMany r = (KR.KnitMany r, P.Member (D.DataCache P.PandocError T.Text BL.ByteString) r)

{-                
cerealStrict :: S.Serialize a => D.Serialize T.Text a BS.ByteString
cerealStrict = D.Serialize S.encode S.decode
-}

cerealLazy :: S.Serialize a => D.Serialize T.Text a BL.ByteString
cerealLazy = D.Serialize S.encodeLazy S.decodeLazy

knitStore :: P.Members '[DataCache e k b, P.Error e] r
