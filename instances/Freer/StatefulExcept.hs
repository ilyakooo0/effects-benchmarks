{-# LANGUAGE TypeApplications, DataKinds #-}
module Freer.StatefulExcept where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Error

countDownExc :: Eff '[State Int, Error String] b
countDownExc = go where
  go = get @Int >>= (\n -> if n <= 0 then throwError "what" else put @Int (n - 1) *> go)
