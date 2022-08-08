module Capability.IncomingRequest where

import Prelude

import Capability.DataContract (class DecodeDataContract, decodeContractJson)
import Capability.Has (class Has, getter)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (jsonParser)
import Data.Bifunctor (lmap)
import Data.Request (RequestMethod)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import FFI.DurableObject (DurableObjectRequest, doRequestGetBody, doRequestGetMethod)

class Monad m <= IncomingRequest m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String
  getBodyJson :: ∀ a b. (DecodeDataContract a b) => m b

instance incomingRequestInstance :: (Has s DurableObjectRequest, MonadAsk s m, MonadAff m, MonadThrow Error m) => IncomingRequest m where
  getRequestMethod = do
    request <- asks getter
    pure <<< doRequestGetMethod $ request
  getBodyString = do
    request <- asks getter
    liftAff <<< doRequestGetBody $ request
  getBodyJson = do
    body <- getBodyString
    parsed <- liftEither <<< lmap error <<< jsonParser $ body
    liftEither <<< decodeContractJson $ parsed
