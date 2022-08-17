module Capability.EventEmitter where

import Prelude

class Monad m <= EventEmitter e m | m -> e where
  emit :: e -> m Unit