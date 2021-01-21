{-# LANGUAGE TypeApplications #-}

module Fused.StatefulExcept where


import Control.Effect.Error
import Control.Effect.State


countDownExc :: (Has (State Int) sig m, Has (Error String) sig m) => m a
countDownExc =
  get @Int >>= (\n -> if n <= 0 then throwError "what" else put @Int (n - 1) *> countDownExc)
