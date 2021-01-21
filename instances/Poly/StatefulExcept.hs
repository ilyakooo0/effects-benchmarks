{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications, DataKinds #-}
module Poly.StatefulExcept where

import Polysemy
import Polysemy.State
import Polysemy.Error

countDownExc :: (Member (State Int) r, Member (Error String) r) => Sem r a
countDownExc = get @Int >>= (\n -> if n <= 0 then throw "what" else put @Int (n - 1) *> countDownExc)
