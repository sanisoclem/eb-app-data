module Capability.IncomingRequest where

import Prelude

import Capability.DataContract (class DecodeDataContract, decodeContractJson)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Argonaut (jsonParser)
import Data.Bifunctor (lmap)
import Data.Request (RequestMethod)
import Effect.Exception (Error, error)

class Monad m <= IncomingRequest m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String

getBodyJson :: ∀ a b m. IncomingRequest m => MonadThrow Error m => DecodeDataContract a b => m b
getBodyJson = do
  body <- getBodyString
  parsed <- liftEither <<< lmap error <<< jsonParser $ body
  liftEither <<< decodeContractJson $ parsed
