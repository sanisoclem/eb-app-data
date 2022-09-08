module EB.DB.Capability.RandomId where

import Prelude

import Data.UUID (genUUID, toString)
import Effect.Class (class MonadEffect, liftEffect)

class RandomId i where
  generate :: String -> i

generateId :: ∀ m a. (RandomId a) => (MonadEffect m) => m a
generateId = do
  uuid <- toString <$> liftEffect genUUID
  pure $ generate uuid