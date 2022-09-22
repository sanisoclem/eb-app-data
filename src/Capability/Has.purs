module EB.DB.Capability.Has where

import Prelude

import Control.Monad.State (class MonadState, gets, modify, modify_)
import Data.Lens (Lens', over, view)


class HasLens s a where
  focus :: Lens' s a

getState :: forall m s a. MonadState s m => HasLens s a => m a
getState = gets (view focus)

modifyState :: forall m s a. MonadState s m => HasLens s a => (a -> a) -> m a
modifyState fn = view focus <$> modify (over focus fn)

modifyState_ :: forall m s a. MonadState s m => HasLens s a => (a -> a) -> m Unit
modifyState_ fn = modify_ (over focus fn)