module Capability.DurableStorage where

import Data.Maybe
import Prelude

import Capability.Codec (class Decodable, class Encodable)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Either (note)
import Effect.Exception (Error, error)

class Monad m <= DurableStorage m where
  tryGetState :: ∀ a. Decodable a => String -> m (Maybe a)
  putState :: ∀ a. Encodable a => String -> a -> m Unit
  deleteState :: String -> m Unit

--   startBatch :: BatchDurableStorage m => m a -> m a

-- class Monad m <= BatchDurableStorage m where
--   batchPutState :: ∀ a b. Encodable a => String -> a -> m Unit
--   batchDeleteState :: String -> m Unit
--   commitBatchState :: m Unit

getState :: ∀ a m. (MonadThrow Error m) => (DurableStorage m) => (Decodable a) => String -> m a
getState key = tryGetState key >>= note (error $ "state not found: " <> key) >>> liftEither

