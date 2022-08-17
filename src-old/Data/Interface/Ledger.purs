module Data.Interface.Ledger where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract)
import Data.Common (AccountId, AccountType, Denomination, Money, TransactionId)
import Data.Contract.Interface.Ledger (LedgerCommandContract(..), LedgerQueryContract(..))
import Data.Maybe (Maybe)

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


instance encodeDataContractLedgerCommand :: EncodeDataContract LedgerCommandContract LedgerCommand where
  toContract = case _ of
    UpdateLedger x -> UpdateLedgerV1 x
    CreateAccount x -> CreateAccountV1 x
    UpdateAccount x -> UpdateAccountV1 x
    CloseAccount x -> CloseAccountV1 x
    CreateTransaction x -> CreateTransactionV1 x
    UpdateTransaction x -> UpdateTransactionV1 x
    DeleteTransaction x -> DeleteTransactionV1 x

instance decodeDataContractLedgerCommand :: DecodeDataContract LedgerCommandContract LedgerCommand where
  fromContract = pure <<< case _ of
    UpdateLedgerV1 x -> UpdateLedger x
    CreateAccountV1 x -> CreateAccount x
    UpdateAccountV1 x -> UpdateAccount x
    CloseAccountV1 x -> CloseAccount x
    CreateTransactionV1 x -> CreateTransaction x
    UpdateTransactionV1 x -> UpdateTransaction x
    DeleteTransactionV1 x -> DeleteTransaction x

instance encodeDataContractLedgerQuery :: EncodeDataContract LedgerQueryContract LedgerQuery where
  toContract = case _ of
    GetLedger -> GetLedgerV1
    GetTransactions -> GetTransactionsV1

instance decodeDataContractLedgerQuery :: DecodeDataContract LedgerQueryContract LedgerQuery where
  fromContract = pure <<< case _ of
    GetLedgerV1 -> GetLedger
    GetTransactionsV1 -> GetTransactions