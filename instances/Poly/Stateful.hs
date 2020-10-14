{-# LANGUAGE DataKinds, TypeApplications #-}

module Poly.Stateful where

import Polysemy
import Polysemy.State as State

type StateM = Sem '[State Int]

get' :: StateM Int
get' = State.get @Int

put' :: Int -> StateM ()
put' = State.put @Int

runStateful :: Int -> StateM a -> (Int, a)
runStateful n x = run $ State.runState n x

countDownPut :: Int -> (Int, Int)
countDownPut start = runStateful start go where
  go = get' >>= (\n -> if n < 0 then pure n else put' (n - 1) *> go)
