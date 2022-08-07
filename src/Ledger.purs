module Ledger where

import Prelude

import AppM (runAppM)
import Capability.IncomingRequest (class IncomingRequest, getBodyJson, getRequestMethod)
import Capability.Storage (class DurableStorage, getDoState)
import Context (mkContext)
import Control.Monad.Error.Class (class MonadError, catchError)
import Control.Promise (Promise, fromAff)
import Data.Ledger (LedgerDocument(..), LedgerRequest(..))
import Data.Request (RequestMethod(..))
import Data.Response (errorResponse, jsonResponse, notFoundResponse, stringResponse)
import Effect (Effect)
import Effect.Exception (Error)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)

fetchMain :: DurableObjectState -> DurableObjectRequest -> Effect (Promise DurableObjectResponse)
fetchMain state req = fromAff $ runAppM (mkContext state req) handler

handler :: ∀ m. (IncomingRequest m) => (DurableStorage m) => (MonadError Error m) => m DurableObjectResponse
handler = catchError go errorResponse
  where
    go = do
      method <- getRequestMethod
      case method of
        POST -> getBodyJson >>= handlerPost
        GET -> handlerGet
        _ -> notFoundResponse "Not found"

handlerPost :: ∀ m. (DurableStorage m) => LedgerRequest -> m DurableObjectResponse
handlerPost (UpdateLedger x) = stringResponse "OK"
handlerPost GetLedger = do
  ledger :: LedgerDocument <- getDoState "ledger"
  jsonResponse ledger

handlerGet :: ∀ m. (Monad m) => m DurableObjectResponse
handlerGet = do
  stringResponse "GetING"