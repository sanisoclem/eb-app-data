module Handlers.Ledger
  ( handleLedgerRequest
  )
  where

import Prelude

import Capability.IncomingRequest (class IncomingRequest, getBodyJson, getRequestMethod)
import Capability.Storage (class DurableStorage, getDoState)
import Control.Monad.Error.Class (class MonadError, catchError)
import Data.Ledger (LedgerCommand, LedgerDocument, LedgerQuery(..), LedgerRequest(..), LedgerSubscription)
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
        POST -> getBodyJson >>= handleRequest
        _ -> notFoundResponse "Not found"

handleRequest :: ∀ m. (DurableStorage m) => LedgerRequest -> m DurableObjectResponse
handleRequest = case _ of
  LedgerCommand x -> handleCommand x
  LedgerQuery x -> handleQuery x
  LedgerSubscription x -> handleSubscription x

handleCommand ∷ ∀ m. Applicative m ⇒ LedgerCommand → m DurableObjectResponse
handleCommand = case _ of
  _ -> stringResponse "OK"

handleQuery ∷ ∀ m. DurableStorage m ⇒ LedgerQuery → m DurableObjectResponse
handleQuery = case _ of
  GetLedger -> do
    ledger :: LedgerDocument <- getDoState "ledger"
    jsonResponse ledger
  _ -> notFoundResponse "not implemented"

handleSubscription ∷ ∀ m. Applicative m ⇒ LedgerSubscription → m DurableObjectResponse
handleSubscription _ = notFoundResponse "not implemented"