module MTL.Stateful where

import qualified Control.Monad.State.Strict as MTL
import Data.Tuple
import Data.Functor.Identity

type StateM = MTL.StateT Int Identity

get :: StateM Int
get = MTL.get

put :: Int -> StateM ()
put = MTL.put

runStateful :: Int -> StateM a -> (Int, a)
runStateful n x = swap (MTL.runState x n)

countDownPut :: Int -> (Int, Int)
countDownPut start = runStateful start go where
  go = get >>= (\n -> if n < 0 then pure n else put (n - 1) *> go)
