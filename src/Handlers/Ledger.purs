module Handlers.Ledger
  ( handleLedgerRequest
  )
  where

import Prelude

import Capability.DataContract (generateId)
import Capability.IncomingRequest (class IncomingRequest, getBodyJson, getRequestMethod)
import Capability.Storage (class DurableStorage, getDoState)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Data.Common (AccountId, AccountType(..), Denomination(..))
import Data.Document.Ledger (LedgerDocument, getAccount, getLedger, putAccount, putLedger)
import Data.Interface.Ledger (LedgerCommand(..), LedgerQuery(..), LedgerRequest(..), LedgerSubscription)
import Data.Request (RequestMethod(..))
import Data.Response (errorResponse, jsonResponse, notFoundResponse, stringResponse)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error)
import FFI.DurableObject (DurableObjectResponse)

handleLedgerRequest :: ∀ m. (IncomingRequest m) => (DurableStorage m) => MonadThrow Error m => (MonadError Error m) => MonadEffect m => m DurableObjectResponse
handleLedgerRequest = catchError go errorResponse
  where
    go = do
      method <- getRequestMethod
      case method of
        POST -> getBodyJson >>= handleRequest
        _ -> notFoundResponse "Not found"

handleRequest :: ∀ m. DurableStorage m => MonadThrow Error m => MonadEffect m => LedgerRequest -> m DurableObjectResponse
handleRequest = case _ of
  LedgerCommand x -> handleCommand x >>= const (stringResponse "OK")
  LedgerQuery x -> handleQuery x
  LedgerSubscription x -> handleSubscription x

handleCommand ∷ ∀ m. DurableStorage m => MonadThrow Error m => MonadEffect m ⇒ LedgerCommand → m Unit -- TODO: return events
handleCommand = case _ of
  UpdateLedger x -> putLedger  =<< _ { name = x.name } <$> getLedger
  CreateAccount x -> do
    accountId <- generateId
    let account = { accountId
      , accountType: x.accountType
      , balance: 0
      , denomination: x.denomination
      , name: x.name
      , closed: false
      }
    putAccount accountId account
  UpdateAccount x -> do
    account <- getAccount x.accountId
    putAccount x.accountId account { name = x.name }
  CloseAccount accountId -> do
    account <- getAccount accountId
    case account.balance of
      0 -> putAccount accountId account { closed = true}
      _ -> throwError $ error "Cannot close account with balance"
  _ -> pure unit

handleQuery ∷ ∀ m. DurableStorage m ⇒ LedgerQuery → m DurableObjectResponse
handleQuery = case _ of
  GetLedger -> do
    ledger :: LedgerDocument <- getDoState "ledger"
    jsonResponse ledger
  _ -> notFoundResponse "not implemented"

handleSubscription ∷ ∀ m. Applicative m ⇒ LedgerSubscription → m DurableObjectResponse
handleSubscription _ = notFoundResponse "not implemented"