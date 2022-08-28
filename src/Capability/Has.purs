module Capability.Has where


-- TODO: can I make a generic instanc of this?
-- since you can't create instances for records, this becomes less useful
class HasGetter a b where
  getter :: b -> a

class HasSetter a b where
  setter :: (a -> a) -> b -> b