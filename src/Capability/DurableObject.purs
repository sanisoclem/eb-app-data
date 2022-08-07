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
  , errorResponse
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
import Effect.Exception (Error, error)
import FFI.DurableObject (DurableObjectResponse, doGetState, doRequestGetBody, doRequestGetMethod, doStringResponse)

class Monad m <= DurableObject m where
  getRequestMethod :: m RequestMethod
  getBodyString :: m String
  getBodyJson :: forall a. (DecodeJson a) => m a
  tryGetDoState :: forall a. (DecodeJson a) => String -> m (Maybe a)
  getDoState :: forall a. (DecodeJson a) => String -> m a

  -- responses
  stringResponse :: String -> m DurableObjectResponse
  jsonResponse :: forall a. (EncodeJson a) => a -> m DurableObjectResponse
  notFoundResponse :: String -> m DurableObjectResponse
  errorResponse :: Error -> m DurableObjectResponse

instance durableObjectState :: (MonadAsk ContextData m, MonadAff m, MonadThrow Error m) => DurableObject m where
  getRequestMethod = do
    request <- asks _.durableObjectRequest
    pure $ doRequestGetMethod request
  getBodyString = do
    request <- asks _.durableObjectRequest
    liftAff $ doRequestGetBody request
  getBodyJson = do
    body <- getBodyString
    parsed <- liftEither $ lmap error $ jsonParser body
    liftEither $ lmap (error <<< printJsonDecodeError) $ decodeJson parsed
  tryGetDoState key = do
    state <- asks _.durableObjectState
    val <- liftAff $ doGetState state key
    sequence $ liftEither <$> lmap (error <<< printJsonDecodeError) <$> decodeJson <$> val
  --getDoState = tryGetDoState >=> note "state not found" >>> liftEither
  getDoState key = tryGetDoState key >>= note (error $ "state not found: " <> key) >>> liftEither

  stringResponse resp =
    pure $ doStringResponse resp 200
  jsonResponse = encodeJson >>> stringify >>> stringResponse
  notFoundResponse msg = do
    pure $ doStringResponse msg 404
  errorResponse err = pure $ doStringResponse (show err) 500
