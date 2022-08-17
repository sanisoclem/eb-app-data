module Capability.Storage where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Either (note)
import Data.Maybe (Maybe)
import Effect.Exception (Error, error)

class Monad m <= DurableStorage m where
  tryGetState :: ∀ a b. (DecodeDataContract a b) => String -> m (Maybe b)
  putState :: ∀ a b. (EncodeDataContract a b) => String -> b -> m Unit
  deleteState :: String -> m Unit

  batchPutState :: ∀ a b. (EncodeDataContract a b) => String -> b -> m Unit
  batchDeleteState :: String -> m Unit
  commitBatchState :: m Unit

getState :: ∀ a b m. (MonadThrow Error m) => (DurableStorage m) => (DecodeDataContract a b) => String -> m b
getState key = tryGetState key >>= note (error $ "state not found: " <> key) >>> liftEither
