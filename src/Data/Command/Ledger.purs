module Data.Command.Ledger where


import Data.Common (AccountId, AccountType, Denomination, TransactionId)
import Data.Maybe (Maybe)
import Data.Money (Money)

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