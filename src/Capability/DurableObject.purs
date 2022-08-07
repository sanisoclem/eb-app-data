module Capability.DurableObject
  ( class DurableObject
  , getRequestMethod
  , stringResponse
  , jsonResponse
  , getBodyString
  , getBodyJson
  , notFoundResponse
  , tryGetDoState
  , getDoState
  )
  where

import Prelude

import Context (ContextData)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, printJsonDecodeError, stringify)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Maybe (Maybe)
import Data.Request (RequestMethod)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff, liftAff)
import FFI.DurableObject (DurableObjectResponse, doGetState, doNotFoundResponse, doRequestGetBody, doRequestGetMethod, doStringResponse)

class Monad m <= DurableObject m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String
  getBodyJson :: forall a. (DecodeJson a) => m a
  tryGetDoState :: forall a. (DecodeJson a) => String -> m (Maybe a)
  getDoState :: forall a. (DecodeJson a) => String -> m a
  stringResponse :: String -> m DurableObjectResponse
  jsonResponse :: forall a. (EncodeJson a) => a -> m DurableObjectResponse
  notFoundResponse :: String -> m DurableObjectResponse

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
    liftEither $ lmap printJsonDecodeError $ decodeJson parsed
  notFoundResponse msg = do
    pure $ doNotFoundResponse msg
  tryGetDoState key = do
    state <- asks _.durableObjectState
    val <- liftAff $ doGetState state key
    sequence $ liftEither <$> lmap printJsonDecodeError <$> decodeJson <$> val
  --getDoState = tryGetDoState >=> note "state not found" >>> liftEither
  getDoState key = tryGetDoState key >>= note ("state not found: " <> key) >>> liftEither