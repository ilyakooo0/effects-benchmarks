{-# LANGUAGE TypeApplications #-}

module Fused.Stateful where

import Control.Carrier.State.Lazy as State
import Data.Functor.Identity

type StateM = StateC Int Identity

get' :: StateM Int
get' = State.get @Int

put' :: Int -> StateM ()
put' = State.put @Int

runStateful :: Int -> StateM a -> (Int, a)
runStateful n x = run $ State.runState n x

countDownPut :: Int -> (Int, Int)
countDownPut start = runStateful start go where
  go = get' >>= (\n -> if n < 0 then pure n else put' (n - 1) *> go)
