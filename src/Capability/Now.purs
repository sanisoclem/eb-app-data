module EB.DB.Capability.Now where

import Prelude

import EB.DB.Data.Instant (Instant, now)
import Effect.Class (class MonadEffect)

class Monad m <= MonadNow m where
  nowUtc :: m Instant

instance MonadEffect m => MonadNow m where
  nowUtc = now