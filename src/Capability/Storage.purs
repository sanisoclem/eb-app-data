module Capability.Storage
  where

import Prelude

import Capability.DataContract (class DataContract, decodeContractJson)
import Capability.Has (class Has, getter)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (class DecodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import FFI.DurableObject (DurableObjectState, doGetState)

class Monad m <= DurableStorage m where
  tryGetDoState :: forall a b. (DecodeJson a) => (DataContract a b) => String -> m (Maybe b)
  getDoState :: forall a b. (DecodeJson a) => (DataContract a b) => String -> m b

instance durableStorageInstance :: (Has s DurableObjectState, MonadAsk s m, MonadAff m, MonadThrow Error m) => DurableStorage m where
  tryGetDoState key = do
    state <- asks getter
    val <- liftAff $ doGetState state key
    sequence $ liftEither <$> lmap (error <<< printJsonDecodeError) <$> decodeContractJson <$> val
  getDoState key = tryGetDoState key >>= note (error $ "state not found: " <> key) >>> liftEither