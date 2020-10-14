module MTL.StatefulExcept where

import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Except as MTL
import Data.Tuple
import Data.Functor.Identity

type StateM = MTL.StateT Int (MTL.ExceptT String Identity)

get :: StateM Int
get = MTL.get

put :: Int -> StateM ()
put = MTL.put

throw :: String -> StateM a
throw = MTL.throwError

runStatefulExcept :: Int -> StateM a -> Either String (Int, a)
runStatefulExcept n x = fmap swap . MTL.runExcept $ MTL.runStateT x n

countDownExc :: Int -> Either String (Int, Int)
countDownExc start = runStatefulExcept start go where
  go = get >>= (\n -> if n <= 0 then throw "what" else put (n - 1) *> go)
