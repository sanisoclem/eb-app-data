module Capability.Has where

class Has a b where
  getter :: a -> b
