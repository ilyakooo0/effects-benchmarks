{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, TypeApplications #-}

module ExtEff.Stateful where

import Control.Eff
import Control.Eff.State.Strict

countDownPut :: Member (State Int) r => Eff r Int
countDownPut = get >>= (\n -> if n < 0 then pure n else put (n - 1) *> countDownPut)
