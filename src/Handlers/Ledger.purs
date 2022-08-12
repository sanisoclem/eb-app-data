module Handlers.Ledger
  ( handleLedgerRequest
  ) where

import Prelude

import Capability.DataContract (generateId)
import Capability.IncomingRequest (class IncomingRequest, getBodyJson, getRequestMethod)
import Capability.Storage (class DurableStorage, commitBatchState, getState)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError)
import Data.Common (AccountId, Money, zeroMoney)
import Data.Document.Ledger (LedgerDocument, creditAccount, debitAccount, deleteTransaction, getAccount, getLedger, getTransaction, postTransaction, putAccount, putLedger, putTransaction)
import Data.Interface.Ledger (LedgerCommand(..), LedgerQuery(..), LedgerRequest(..), LedgerSubscription)
import Data.Maybe (Maybe)
import Data.Request (RequestMethod(..))
import Data.Response (errorResponse, jsonResponse, notFoundResponse, stringResponse)
import Data.Traversable (sequence)
import Data.Utility (ensure)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import FFI.DurableObject (DurableObjectResponse)

handleLedgerRequest
  :: ∀ m
   . IncomingRequest m
  => DurableStorage m
  => MonadThrow Error m
  => MonadError Error m
  => MonadEffect m
  => m DurableObjectResponse
handleLedgerRequest = catchError go errorResponse
  where
  go = do
    method <- getRequestMethod
    case method of
      POST -> getBodyJson >>= handleRequest
      _ -> notFoundResponse "Not found"

handleRequest
  :: ∀ m
   . DurableStorage m
  => MonadThrow Error m
  => MonadEffect m
  => LedgerRequest
  -> m DurableObjectResponse
handleRequest = case _ of
  LedgerCommand x -> do
    handleCommand x
    commitBatchState
    stringResponse "OK"
  LedgerQuery x -> handleQuery x
  LedgerSubscription x -> handleSubscription x

handleCommand
  :: ∀ m
   . DurableStorage m
  => MonadThrow Error m
  => MonadEffect m
  => LedgerCommand
  -> m Unit -- TODO: return events
handleCommand = case _ of
  UpdateLedger x -> putLedger =<< _ { name = x.name } <$> getLedger
  CreateAccount x -> do
    accountId <- generateId
    putAccount
      { accountId
      , accountType: x.accountType
      , balance: zeroMoney
      , denomination: x.denomination
      , name: x.name
      , closed: false
      }
  UpdateAccount x -> do
    account <- getAccount x.accountId
    putAccount account { name = x.name }
  CloseAccount accountId -> do
    account <- getAccount accountId
    ensure "Account balance should be zero" $ account.balance == zeroMoney
    putAccount account { closed = true }
  CreateTransaction x -> do
    transactionId <- generateId
    postTransaction
      { transactionId
      , sortKey: x.sortKey
      , credit: x.credit
      , debit: x.debit
      , amount: x.amount
      , notes: x.notes
      }
    getDebitPut x.debit x.amount
    getCreditPut x.credit x.amount
  UpdateTransaction x -> do
    prevTrans <- getTransaction x.transactionId
    -- reverse prev transaction
    getDebitPut prevTrans.credit prevTrans.amount
    getCreditPut prevTrans.debit prevTrans.amount

    let updatedTrans = prevTrans { amount = x.amount
      , notes = x.notes
      , sortKey = x.sortKey
      , debit = x.debit
      , credit = x.credit
    }

    putTransaction updatedTrans
    getDebitPut updatedTrans.debit updatedTrans.amount
    getCreditPut updatedTrans.credit updatedTrans.amount
  DeleteTransaction transactionId -> do
    trans <- getTransaction transactionId
    getDebitPut trans.credit trans.amount
    getCreditPut trans.debit trans.amount
    deleteTransaction transactionId

getDebitPut :: forall m. MonadThrow Error m => DurableStorage m => Maybe AccountId -> Money -> m Unit
getDebitPut maybeAccount amount = void <<< sequence $ (putAccount <<< debitAccount amount <=< getAccount) <$> maybeAccount

getCreditPut :: forall m. MonadThrow Error m => DurableStorage m => Maybe AccountId -> Money -> m Unit
getCreditPut maybeAccount amount = void <<< sequence $ (putAccount <<< creditAccount amount <=< getAccount) <$> maybeAccount

handleQuery
  :: ∀ m
   . DurableStorage m
  => MonadThrow Error m
  => LedgerQuery
  -> m DurableObjectResponse
handleQuery = case _ of
  GetLedger -> do
    ledger :: LedgerDocument <- getState "ledger"
    jsonResponse ledger
  _ -> notFoundResponse "not implemented"

handleSubscription
  :: ∀ m
   . Applicative m
  => LedgerSubscription
  -> m DurableObjectResponse
handleSubscription _ = notFoundResponse "not implemented"