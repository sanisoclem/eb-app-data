module Capability.Has where

class Has a b where
  getter :: a -> b

class HasSetter a b where
  setter :: (b -> b) -> a -> a