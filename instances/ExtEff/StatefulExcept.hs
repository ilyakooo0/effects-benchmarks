{-# LANGUAGE TypeApplications, DataKinds #-}
module ExtEff.StatefulExcept where

import Control.Eff
import Control.Eff.State.Strict as State
import Control.Eff.Exception
import Data.Tuple

type StateM = Eff '[State Int, Exc String]

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
