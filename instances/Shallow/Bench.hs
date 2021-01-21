module Shallow.Bench
  ( countDownBench
  , countDownExcBench
  , httpBench
  ) where

import Shallow.Stateful
import Shallow.StatefulExcept
import Shallow.HTTP

countDownBench :: Int -> (Int, Int)
countDownBench = countDownPut

countDownExcBench :: Int -> Either String (Int, Int)
countDownExcBench = countDownExc

httpBench :: Int -> IO Int
httpBench = doHTTP
