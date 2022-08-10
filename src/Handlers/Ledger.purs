module Handlers.Ledger
  ( handleLedgerRequest
  )
  where

import Prelude

import Capability.IncomingRequest (class IncomingRequest, getBodyJson, getRequestMethod)
import Capability.Storage (class DurableStorage, getDoState, putDoState)
import Control.Monad.Error.Class (class MonadError, catchError)
import Data.Document.Ledger (LedgerDocument(..), getLedger, putLedger)
import Data.Interface.Ledger (LedgerCommand(..), LedgerQuery(..), LedgerRequest(..), LedgerSubscription)
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

handleRequest :: ∀ m. DurableStorage m => LedgerRequest -> m DurableObjectResponse
handleRequest = case _ of
  LedgerCommand x -> handleCommand x
  LedgerQuery x -> handleQuery x
  LedgerSubscription x -> handleSubscription x

handleCommand ∷ ∀ m. DurableStorage m ⇒ LedgerCommand → m DurableObjectResponse
handleCommand = case _ of
  UpdateLedger x -> do
    ledger <- getLedger
    putLedger ledger { name = x.name }
    stringResponse "OK" -- TODO: return events
  _ -> stringResponse "OK"

handleQuery ∷ ∀ m. DurableStorage m ⇒ LedgerQuery → m DurableObjectResponse
handleQuery = case _ of
  GetLedger -> do
    ledger :: LedgerDocument <- getDoState "ledger"
    jsonResponse ledger
  _ -> notFoundResponse "not implemented"

handleSubscription ∷ ∀ m. Applicative m ⇒ LedgerSubscription → m DurableObjectResponse
handleSubscription _ = notFoundResponse "not implemented"