{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications, DataKinds #-}

module Freer.Stateful where

import Control.Monad.Freer
import Control.Monad.Freer.State as State

countDownPut :: Member (State Int) r => Eff r Int
countDownPut = get >>= (\n -> if n < 0 then pure n else put (n - 1) *> countDownPut)
