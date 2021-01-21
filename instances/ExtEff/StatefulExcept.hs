{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications, DataKinds #-}
module ExtEff.StatefulExcept where

import Control.Eff
import Control.Eff.State.Strict
import Control.Eff.Exception

countDownExc :: (Member (State Int) r, Member (Exc String) r) => Eff r b
countDownExc = get @Int >>= (\n -> if n <= 0 then throwError "what" else put @Int (n - 1) *> countDownExc)
