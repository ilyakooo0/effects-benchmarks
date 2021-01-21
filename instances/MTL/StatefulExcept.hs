{-# LANGUAGE FlexibleContexts #-}
module MTL.StatefulExcept where

import Control.Monad.State.Class
import Control.Monad.Error.Class

countDownExc :: (MonadState Int m, MonadError String m) => m a
countDownExc = get >>= (\n -> if n <= 0 then throwError "what" else put (n - 1) *> countDownExc)
