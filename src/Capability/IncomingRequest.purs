module Capability.IncomingRequest where

import Prelude

import Capability.DataContract (class DataContract, decodeContractJson)
import Capability.Has (class Has, getter)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (class DecodeJson, jsonParser, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Request (RequestMethod)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import FFI.DurableObject (DurableObjectRequest, doRequestGetBody, doRequestGetMethod)

class Monad m <= IncomingRequest m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String
  getBodyJson :: forall a b. (DecodeJson a) => (DataContract a b) => m b

instance incomingRequestInstance :: (Has s DurableObjectRequest, MonadAsk s m, MonadAff m, MonadThrow Error m) => IncomingRequest m where
  getRequestMethod = do
    request <- asks getter
    pure $ doRequestGetMethod request
  getBodyString = do
    request <- asks getter
    liftAff $ doRequestGetBody request
  getBodyJson = do
    body <- getBodyString
    parsed <- liftEither $ lmap error $ jsonParser body
    liftEither $ lmap (error <<< printJsonDecodeError) $ decodeContractJson parsed
