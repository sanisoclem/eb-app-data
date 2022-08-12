module Data.Interface.Ledger where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract)
import Data.Common (AccountId, AccountType, Denomination, Money(..), SubscriptionId, TransactionId)
import Data.Contract.Interface.Ledger (LedgerRequestContract(..))
import Data.Maybe (Maybe)

data LedgerRequest
  = LedgerQuery LedgerQuery
  | LedgerCommand LedgerCommand
  | LedgerSubscription LedgerSubscription

data LedgerCommand
  = UpdateLedger
      { name :: String
      }
  | CreateAccount
      { name :: String
      , accountType :: AccountType
      , denomination :: Denomination
      }
  | UpdateAccount
      { accountId :: AccountId
      , name :: String
      }
  | CloseAccount AccountId
  | CreateTransaction
      { sortKey :: Int
      , credit :: Maybe AccountId
      , debit :: Maybe AccountId
      , amount :: Money
      , notes :: String
      }
  | UpdateTransaction
      { sortKey :: Int
      , transactionId :: TransactionId
      , credit :: Maybe AccountId
      , debit :: Maybe AccountId
      , amount :: Money
      , notes :: String
      }
  | DeleteTransaction TransactionId

data LedgerQuery
  = GetLedger
  | GetTransactions

data LedgerSubscription
  = Subscribe String -- TODO must be https uri
  | Unsubscribe SubscriptionId

instance encodeDataContractLedgerRequest :: EncodeDataContract LedgerRequestContract LedgerRequest where
  toContract (LedgerCommand cmd) = case cmd of
    UpdateLedger x -> UpdateLedgerV1 x
    CreateAccount x -> CreateAccountV1 x
    UpdateAccount x -> UpdateAccountV1 x
    CloseAccount x -> CloseAccountV1 x
    CreateTransaction x -> CreateTransactionV1 x
    UpdateTransaction x -> UpdateTransactionV1 x
    DeleteTransaction x -> DeleteTransactionV1 x
  toContract (LedgerQuery qry) = case qry of
    GetLedger -> GetLedgerV1
    GetTransactions -> GetTransactionsV1
  toContract (LedgerSubscription s) = case s of
    Subscribe x -> SubscribeV1 x
    Unsubscribe x -> UnsubscribeV1 x

instance decodeDataContractLedgerRequest :: DecodeDataContract LedgerRequestContract LedgerRequest where
  fromContract = pure <<< case _ of
    UpdateLedgerV1 x -> LedgerCommand $ UpdateLedger x
    CreateAccountV1 x -> LedgerCommand $ CreateAccount x
    UpdateAccountV1 x -> LedgerCommand $ UpdateAccount x
    CloseAccountV1 x -> LedgerCommand $ CloseAccount x
    CreateTransactionV1 x -> LedgerCommand $ CreateTransaction x
    UpdateTransactionV1 x -> LedgerCommand $ UpdateTransaction x
    DeleteTransactionV1 x -> LedgerCommand $ DeleteTransaction x
    GetLedgerV1 -> LedgerQuery GetLedger
    GetTransactionsV1 -> LedgerQuery GetTransactions
    SubscribeV1 x -> LedgerSubscription $ Subscribe x
    UnsubscribeV1 x -> LedgerSubscription $ Unsubscribe x
