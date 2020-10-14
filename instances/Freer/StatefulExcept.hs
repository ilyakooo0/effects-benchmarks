{-# LANGUAGE TypeApplications, DataKinds #-}
module Freer.StatefulExcept where

import Control.Monad.Freer
import Control.Monad.Freer.State as State
import Control.Monad.Freer.Error
import Data.Tuple

type StateM = Eff '[State Int, Error String]

get' :: StateM Int
get' = State.get @Int

put' :: Int -> StateM ()
put' = State.put @Int

throw :: String -> StateM a
throw = throwError

runStatefulExcept :: Int -> StateM a -> Either String (Int, a)
runStatefulExcept n x = fmap swap . run . runError $ State.runState n x

countDownExc :: Int -> Either String (Int, Int)
countDownExc start = runStatefulExcept start go where
  go = get' >>= (\n -> if n <= 0 then throw "what" else put' (n - 1) *> go)
