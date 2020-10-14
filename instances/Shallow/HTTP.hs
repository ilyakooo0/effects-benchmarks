{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shallow.HTTP where

import Control.Monad

newtype HttpM a = HttpM { runHttp :: IO a}
  deriving (Applicative, Functor, Monad)

get' :: HttpM String
get' = pure "lmao"

post' :: String -> HttpM String
post' s = pure ("posted " <> s)

close' :: HttpM ()
close' = pure ()

open' :: String -> HttpM ()
open' _ = pure ()

doHTTP :: Int -> IO Int
doHTTP n = runHttp $ do
  open' "cats"
  replicateM_ n (get' *> post' "cats")
  close'
  pure n
