module Capability.Storage.Outbox where

import Prelude
import Control.Monad.Trans.Class (class MonadTrans, lift)

class Monad m <= MonadOutbox e m | m -> e where
  queue :: e -> m Unit

instance monadOutboxMonadTrans :: (Monad (t m), MonadOutbox e m, MonadTrans t) => MonadOutbox e (t m) where
  queue = lift <<< queue