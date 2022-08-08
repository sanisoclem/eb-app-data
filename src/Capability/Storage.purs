module Capability.Storage
  where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract, decodeContractJson, encodeContractJson)
import Capability.Has (class Has, getter)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (note)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import FFI.DurableObject (DurableObjectState, doGetState, doPutState, doDeleteState)


class Monad m <= DurableStorage m where
  tryGetDoState :: ∀ a b. (DecodeDataContract a b) => String -> m (Maybe b)
  getDoState :: ∀ a b. (DecodeDataContract a b) => String -> m b
  putDoState :: ∀ a b. (EncodeDataContract a b) => String -> b -> m Unit
  deleteDoState :: String -> m Unit

  -- batchPutDoState :: ∀ a b. (DataContract a b) => String -> b -> m Unit
  -- batchDeleteDoState :: String -> m Unit
  -- commitBatch :: m Unit

instance durableStorageInstance :: (Has s DurableObjectState, MonadAsk s m, MonadAff m, MonadThrow Error m) => DurableStorage m where
  tryGetDoState key = do
    state <- asks getter
    val <- liftAff $ doGetState state key
    sequence $ liftEither <<< decodeContractJson <$> val
  getDoState key = tryGetDoState key >>= note (error $ "state not found: " <> key) >>> liftEither
  putDoState key value = do
    state <- asks getter
    liftAff <<< doPutState state key <<< encodeContractJson $ value
  deleteDoState key = liftAff <<< (flip doDeleteState) key =<< asks getter
