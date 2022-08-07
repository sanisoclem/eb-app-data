module Capability.DurableObject
  ( class DurableObject
  , getRequestMethod
  , stringResponse
  , jsonResponse
  , getBodyString
  , getBodyJson
  )
  where

import Prelude

import Context (ContextData)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, printJsonDecodeError, stringify)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Request (RequestMethod)
import Effect.Aff.Class (class MonadAff, liftAff)
import FFI.DurableObject (DurableObjectResponse, doRequestGetMethod, doStringResponse, doRequestGetBody)

class Monad m <= DurableObject m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String
  getBodyJson :: forall a. (DecodeJson a) => m (Either String a)
  stringResponse :: String -> m DurableObjectResponse
  jsonResponse :: forall a. (EncodeJson a) => a -> m DurableObjectResponse

instance durableObjectState :: (MonadAsk ContextData m, MonadAff m, MonadThrow String m) => DurableObject m where
  getRequestMethod = do
    request <- asks _.durableObjectRequest
    pure $ doRequestGetMethod request
  stringResponse resp =
    pure $ doStringResponse resp
  jsonResponse = encodeJson >>> stringify >>> stringResponse
  getBodyString = do
    request <- asks _.durableObjectRequest
    liftAff $ doRequestGetBody request
  getBodyJson = do
    body <- getBodyString
    parsed <- liftEither $ jsonParser body
    liftEither $ lmap printJsonDecodeError (decodeJson parsed)