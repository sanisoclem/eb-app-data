module Data.Ledger where

import Capability.DataContract

import Data.Common (Instant)
import DataContract.Ledger (LedgerDocumentContract(..), LedgerRequestContract(..))

data LedgerDocument = LedgerDocument
  { name :: String
  , createdAt :: Instant
  }

instance contractLedgerDocument :: DataContract LedgerDocumentContract LedgerDocument where
  fromContract (LedgerDocumentContractV1 d) = LedgerDocument d
  toContract (LedgerDocument x) = LedgerDocumentContractV1 x

data LedgerRequest
  = LedgerQuery LedgerQuery
  | LedgerCommand LedgerCommand
  | LedgerSubscription LedgerSubscription

data LedgerCommand
  = UpdateLedger
  | CreateAccount
  | UpdateAccount
  | CloseAccount
  | CreateTransaction
  | UpdateTransaction
  | DeleteTransaction

data LedgerQuery
  = GetLedger
  | GetAccounts
  | GetTransactions

data LedgerSubscription
  = Subscribe
  | Unsubscribe


instance dataContractLedgerRequest :: DataContract LedgerRequestContract LedgerRequest where
  toContract LedgerQuery = GetLedgerV1
  toContract (UpdateLedger x) = UpdateLedgerV1 x

  fromContract GetLedgerV1 = GetLedger
  fromContract (UpdateLedgerV1 x) = UpdateLedger x
