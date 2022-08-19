module Data.Command.Ledger where

import Prelude

import Capability.Codec (class Decodable, class Encodable)
import Capability.Utility (convertJsonErrorToError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Common (AccountId, AccountType, Denomination, TransactionId)
import Data.Generic.Rep (class Generic)
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

derive instance Generic LedgerCommand _
instance Encodable LedgerCommand where
  encode = genericEncodeJson
instance Decodable LedgerCommand where
  decode = convertJsonErrorToError <<< genericDecodeJson