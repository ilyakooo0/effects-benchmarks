{-# LANGUAGE TypeApplications #-}

module Fused.Stateful where

import Control.Effect.State

countDownPut :: Has (State Int) sig m => m Int
countDownPut = get >>= (\n -> if n < 0 then pure n else put (n - 1) *> countDownPut)
