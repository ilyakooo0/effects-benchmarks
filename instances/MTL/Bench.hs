module MTL.Bench
  ( countDownBench
  , countDownExcBench
  , httpBench
  ) where

import MTL.Stateful
import MTL.StatefulExcept
import MTL.HTTP
import Control.Monad.State.Strict
import Control.Monad.Except

countDownBench :: Int -> (Int, Int)
countDownBench start = flip runState start $ countDownPut

countDownExcBench :: Int -> Either String (Int, Int)
countDownExcBench start = runExcept . flip runStateT start $ countDownExc

httpBench :: Int -> IO Int
httpBench n = runHttp (doHTTP n)
