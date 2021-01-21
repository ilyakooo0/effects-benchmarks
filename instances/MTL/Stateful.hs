{-# LANGUAGE FlexibleContexts #-}
module MTL.Stateful where

import Control.Monad.State.Class

countDownPut :: MonadState Int m => m Int
countDownPut = get >>= (\n -> if n < 0 then pure n else put (n - 1) *> countDownPut)
