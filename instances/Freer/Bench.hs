module Freer.Bench
  ( countDownBench
  , countDownExcBench
  , httpBench
  ) where

import Freer.Stateful
import Freer.StatefulExcept
import Freer.HTTP
import Control.Monad.Freer.State
import Control.Monad.Freer.Error
import Control.Monad.Freer

countDownBench :: Int -> (Int, Int)
countDownBench start = run . runState start $ countDownPut

countDownExcBench :: Int -> Either String (Int, Int)
countDownExcBench start = run . runError . runState start $ countDownExc

httpBench :: Int -> IO Int
httpBench n = runHttp (doHTTP n)
