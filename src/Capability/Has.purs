module Capability.Has where

class HasGetter a b where
  getter :: b -> a

class HasSetter a b where
  setter :: (a -> a) -> b -> b