{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Knit.Kernmantle.Report
  (
    module Knit.Kernmantle
  , module Knit.Kernmantle.Report
  ) where

import Knit.Kernmantle
import qualified Knit.Report as K

import qualified Knit.Effect.PandocMonad       as PM
import qualified Knit.Effect.UnusedId          as KUI
import qualified Graphics.Vega.VegaLite        as GV

import qualified Control.Kernmantle.Rope as Rope
import qualified Polysemy                      as P
import qualified Control.Arrow as A

import qualified Data.Text as T

logLE :: K.LogSeverity -> T.Text -> KnitPipeline knitEffs () ()
logLE ls lt = proc _ -> do
  Rope.strand #log LogText -< (ls, lt)

-- add to document
addHvega :: ( PM.PandocEffects r
            , P.Member K.ToPandoc r
            , P.Member KUI.UnusedId r)
         => Maybe T.Text
         -> Maybe T.Text
         -> GV.VegaLite
         -> DocPipeline r () T.Text
addHvega idTextM captionTextM vl =
  let getId = maybe (KUI.getNextUnusedId "figure") return
  in proc _ -> do
    Rope.strand #doc Require -< K.VegaSupport
    idText <- (Rope.strand #knitCore $ A.Kleisli getId) -< idTextM
    A.returnA -< idText

  
