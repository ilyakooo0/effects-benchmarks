module Fused.Bench
  ( countDownBench
  , countDownExcBench
  , httpBench
  ) where

import Fused.Stateful
import Fused.StatefulExcept
import Fused.HTTP
import Control.Carrier.State.Strict
import Control.Carrier.Error.Either

countDownBench :: Int -> (Int, Int)
countDownBench start = run . runState start $ countDownPut

countDownExcBench :: Int -> Either String (Int, Int)
countDownExcBench start = run . runError . runState start $ countDownExc

httpBench :: Int -> IO Int
httpBench n = runHttp (doHTTP n)
