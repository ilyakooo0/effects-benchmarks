module ExtEff.Bench
  ( countDownBench
  , countDownExcBench
  , httpBench
  ) where

import ExtEff.Stateful
import ExtEff.StatefulExcept
import ExtEff.HTTP
import Control.Eff.State.Strict
import Control.Eff.Exception
import Control.Eff

countDownBench :: Int -> (Int, Int)
countDownBench start = run . runState start $ countDownPut

countDownExcBench :: Int -> Either String (Int, Int)
countDownExcBench start = run . runError . runState start $ countDownExc

httpBench :: Int -> IO Int
httpBench n = runHttp (doHTTP n)
