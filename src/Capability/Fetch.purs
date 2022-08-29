module Capability.Fetch where

import Prelude

import Capability.Utility (convertJsonErrorToError)
import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Fetch (RequestMethod)
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Effect.Exception (Error, error)

class Monad m <= MonadFetchRequest m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String
  getPath :: m String
  tryGetParam :: String -> m (Maybe String)

getBodyJson :: ∀ a m. MonadFetchRequest m => MonadThrow Error m => DecodeJson a => m a
getBodyJson = do
  body <- getBodyString
  parsed <- liftEither <<< lmap error <<< jsonParser $ body
  liftEither <<< convertJsonErrorToError <<< decodeJson $ parsed

getParam ::  ∀ m. MonadFetchRequest m => MonadThrow Error m => String -> m String
getParam key = do
  mParam <- tryGetParam key
  liftEither <<< note (error $ "searchParam " <> key <> " not found") $ mParam

tryGetParamInt ::  ∀ m. MonadFetchRequest m => String -> m (Maybe Int)
tryGetParamInt k = bindFlipped fromString <$> tryGetParam k

getParamInt ::  ∀ m. MonadFetchRequest m => MonadThrow Error m => String -> m Int
getParamInt key = liftEither <<< note (error "invalid Int") <<< fromString <=< getParam $ key

class MonadFetchRequest m <= MonadFromRequest m a where
  fromRequest :: m a