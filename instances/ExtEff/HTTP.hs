{-# LANGUAGE DataKinds, TypeOperators, GADTs, FlexibleInstances, MultiParamTypeClasses #-}

module ExtEff.HTTP where

import Control.Eff
import Control.Eff.Extend
import Data.Function
import Control.Monad

type HttpM = Eff '[HTTP, Lift IO]

data HTTP a where
  Open :: String -> HTTP ()
  Close :: HTTP ()
  Post  :: String -> HTTP String
  HGet   :: HTTP String

open' :: String -> HttpM ()
open'  = send . Open

close' :: HttpM ()
close' = send Close

post' :: String -> HttpM String
post' = send . Post

get' :: HttpM String
get' = send HGet

instance Handle HTTP r b (Eff e b) where
  handle h q req = case req of
    Open _s -> h (q ^$ ())
    Close   -> h (q ^$ ())
    Post s  -> h (q ^$ ("posted " <> s))
    HGet    -> h (q ^$ "contents")

doHttp :: Eff (HTTP ': e) b -> Eff e b
doHttp = fix (handle_relay pure)

runHttp :: HttpM a -> IO a
runHttp = runLift . doHttp

doHTTP :: Int -> Eff '[HTTP, Lift IO] Int
doHTTP n = do
  open' "cats"
  replicateM_ n (get' *> post' "cats")
  close'
  pure n
