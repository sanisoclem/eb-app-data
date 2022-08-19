module Capability.Fetch where

import Prelude

import Capability.Codec (class Decodable, decode)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Argonaut (jsonParser)
import Data.Bifunctor (lmap)
import Data.Fetch (RequestMethod)
import Effect.Exception (Error, error)

class Monad m <= MonadFetchRequest m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String

getBodyJson :: ∀ a m. MonadFetchRequest m => MonadThrow Error m => Decodable a => m a
getBodyJson = do
  body <- getBodyString
  parsed <- liftEither <<< lmap error <<< jsonParser $ body
  liftEither <<< decode $ parsed
