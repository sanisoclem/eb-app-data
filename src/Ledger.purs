module Handlers.Ledger
  ( handleCommand
  , handleQuery
  ) where

import Prelude

import Capability.DataContract (generateId)
import Capability.Storage (class DurableStorage, getState)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Common (AccountId, Money, zeroMoney)
import Data.Document.Ledger (LedgerDocument, creditAccount, debitAccount, deleteTransaction, getAccount, getLedger, getTransaction, postTransaction, putAccount, putLedger, putTransaction)
import Data.Event.Ledger (LedgerEvent)
import Data.Interface.Ledger (LedgerCommand(..), LedgerQuery(..))
import Data.Maybe (Maybe(..))
import Data.Response (Response, contractResponse, notFoundResponse)
import Data.Traversable (sequence)
import Data.Utility (ensure)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

handleCommand
  :: ∀ m
   . DurableStorage m
  => MonadThrow Error m
  => MonadEffect m
  => LedgerCommand
  -> m (Maybe LedgerEvent)
handleCommand = case _ of
  UpdateLedger x -> do
    putLedger =<< _ { name = x.name } <$> getLedger
    pure Nothing
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
    pure Nothing
  UpdateAccount x -> do
    account <- getAccount x.accountId
    putAccount account { name = x.name }
    pure Nothing
  CloseAccount accountId -> do
    account <- getAccount accountId
    ensure "Account balance should be zero" $ account.balance == zeroMoney
    putAccount account { closed = true }
    pure Nothing
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
    pure Nothing
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
    pure Nothing
  DeleteTransaction transactionId -> do
    trans <- getTransaction transactionId
    getDebitPut trans.credit trans.amount
    getCreditPut trans.debit trans.amount
    deleteTransaction transactionId
    pure Nothing

getDebitPut :: forall m. MonadThrow Error m => DurableStorage m => Maybe AccountId -> Money -> m Unit
getDebitPut maybeAccount amount = void <<< sequence $ (putAccount <<< debitAccount amount <=< getAccount) <$> maybeAccount

getCreditPut :: forall m. MonadThrow Error m => DurableStorage m => Maybe AccountId -> Money -> m Unit
getCreditPut maybeAccount amount = void <<< sequence $ (putAccount <<< creditAccount amount <=< getAccount) <$> maybeAccount

handleQuery
  :: ∀ m
   . DurableStorage m
  => MonadThrow Error m
  => LedgerQuery
  -> m Response
handleQuery = case _ of
  GetLedger -> do
    ledger :: LedgerDocument <- getState "ledger"
    pure $ contractResponse 200 ledger
  _ -> pure $ notFoundResponse "not implemented"