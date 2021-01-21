module Poly.Bench
  ( countDownBench
  , countDownExcBench
  , httpBench
  ) where

import Poly.Stateful
import Poly.StatefulExcept
import Poly.HTTP
import Polysemy.State
import Polysemy
import Polysemy.Error

countDownBench :: Int -> (Int, Int)
countDownBench start = run . runState start $ countDownPut

countDownExcBench :: Int -> Either String (Int, Int)
countDownExcBench start = run . runError . runState start $ countDownExc

httpBench :: Int -> IO Int
httpBench n = runHttp (doHTTP n)
