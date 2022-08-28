module Capability.Now where

import Prelude
import Data.Instant (Instant)

class Monad m <= MonadNow m where
  nowUtc :: m Instant
