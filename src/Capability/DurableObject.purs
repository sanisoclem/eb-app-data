module Capability.DurableObject
  ( class DurableObject
  , getRequestMethod
  , stringResponse
  )
  where

import Prelude

import Context (ContextData)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Request (RequestMethod)
import FFI.DurableObject (DurableObjectResponse, doRequestGetMethod, doStringResponse)

class Monad m <= DurableObject m where
  getRequestMethod :: m RequestMethod
  stringResponse :: String -> m DurableObjectResponse

instance durableObjectState :: MonadAsk ContextData m => DurableObject m where
  getRequestMethod = do
    request <- asks _.durableObjectRequest
    pure $ doRequestGetMethod request
  stringResponse resp =
    pure $ doStringResponse resp