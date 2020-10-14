{-# LANGUAGE TypeApplications, DataKinds #-}

module Freer.Stateful where

import Control.Monad.Freer
import Control.Monad.Freer.State as State
import Data.Tuple

type StateM = Eff '[State Int]

get' :: StateM Int
get' = State.get @Int

put' :: Int -> StateM ()
put' = State.put @Int

runStateful :: Int -> StateM a -> (Int, a)
runStateful n x = swap . run $ State.runState n x

countDownPut :: Int -> (Int, Int)
countDownPut start = runStateful start go where
  go = get' >>= (\n -> if n < 0 then pure n else put' (n - 1) *> go)
