{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Knit.Report.Error
Description : knit-haskell functions to handle and raise errors in knit-haskell reports.
Copyright   : (c) Adam Conner-Sax 2019
License     : BSD-3-Clause
Maintainer  : adam_conner_sax@yahoo.com
Stability   : experimental

This module has various combinators for simplifying the throwing of Errors in knit-haskell.

<https://github.com/adamConnerSax/knit-haskell/tree/master/examples Examples> are available, and might be useful for seeing how all this works.

-}
module Knit.Report.Error
  (
    -- * Error combinators
    knitError
  , knitMaybe
  , knitEither
  , knitMapError
  )
where

import qualified Knit.Report.EffectStack       as K
import qualified Text.Pandoc.Error             as PA
import           Knit.Effect.PandocMonad        ( textToPandocText )

import qualified Data.Text                     as T

import qualified Polysemy                      as P
import qualified Polysemy.Error                as PE



-- | Throw an error with a specific message.  This will emerge as a 'PandocSomeError' in order
-- to avoid complicating the error type.
-- NB: The Member constraint is satisfied by KnitEffectStack m.
knitError :: P.Member (PE.Error PA.PandocError) r => T.Text -> P.Sem r a
knitError msg =
  PE.throw (PA.PandocSomeError $ textToPandocText $ "Knit User Error: " <> msg)

-- | Throw on 'Nothing' with given message.  This will emerge as a 'PandocSomeError' in order
-- to avoid complicating the error type.
knitMaybe
  :: P.Member (PE.Error PA.PandocError) r => T.Text -> Maybe a -> P.Sem r a
knitMaybe msg = maybe (knitError msg) return

-- | Throw on 'Left' with message.  This will emerge as a 'PandocSomeError' in order
-- to avoid complicating the error type.
knitEither
  :: P.Member (PE.Error PA.PandocError) r => Either T.Text a -> P.Sem r a
knitEither = either knitError return

-- | Map an error type, @e, into a 'PandocError' so it will be handled in this stack
knitMapError
  :: forall e c k ct r a
   . K.KnitEffects c k ct r
  => (e -> T.Text)
  -> P.Sem (PE.Error e ': r) a
  -> P.Sem r a
knitMapError f = PE.mapError $ PA.PandocSomeError . textToPandocText . f
