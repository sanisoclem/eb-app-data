module Handlers.Ledger
  ( handleLedgerRequest
  )
  where

import Prelude

import Capability.IncomingRequest (class IncomingRequest, getBodyJson, getRequestMethod)
import Capability.Storage (class DurableStorage, getDoState)
import Control.Monad.Error.Class (class MonadError, catchError)
import Data.Ledger (LedgerDocument, LedgerRequest(..))
import Data.Request (RequestMethod(..))
import Data.Response (errorResponse, jsonResponse, notFoundResponse, stringResponse)
import Effect.Exception (Error)
import FFI.DurableObject (DurableObjectResponse)

handleLedgerRequest :: ∀ m. (IncomingRequest m) => (DurableStorage m) => (MonadError Error m) => m DurableObjectResponse
handleLedgerRequest = catchError go errorResponse
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