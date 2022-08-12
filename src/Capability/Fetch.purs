module Capability.IncomingRequest where

import Prelude

import Capability.DataContract (class DecodeDataContract, decodeContractJson)
import Capability.Has (class Has, getter)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.State (class MonadState, gets)
import Data.Argonaut (jsonParser)
import Data.Bifunctor (lmap)
import Data.Request (RequestMethod)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import FFI.DurableObject (DurableObjectRequest, doRequestGetBody, doRequestGetMethod)

class Monad m <= IncomingRequest m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String


instance incomingRequestInstance :: (Has s DurableObjectRequest, MonadState s m, MonadAff m, MonadThrow Error m) => IncomingRequest m where
  getRequestMethod = do
    request <- gets getter
    pure <<< doRequestGetMethod $ request
  getBodyString = do
    request <- gets getter
    liftAff <<< doRequestGetBody $ request

getBodyJson :: ∀ a b m. IncomingRequest m => MonadThrow Error m => DecodeDataContract a b => m b
getBodyJson = do
  body <- getBodyString
  parsed <- liftEither <<< lmap error <<< jsonParser $ body
  liftEither <<< decodeContractJson $ parsed
