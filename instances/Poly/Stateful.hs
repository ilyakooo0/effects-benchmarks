{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, TypeApplications #-}

module Poly.Stateful where

import Polysemy
import Polysemy.State

countDownPut :: Member (State Int) r => Sem r Int
countDownPut = get >>= (\n -> if n < 0 then pure n else put (n - 1) *> countDownPut)
