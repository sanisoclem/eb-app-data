module Data.Document.Ledger
  ( LedgerDocument(..)
  , getAccount
  , getLedger
  , putAccount
  , putLedger
  )
  where

import Prelude

import Capability.DataContract (class DecodeDataContract, class EncodeDataContract, toDocumentId)
import Capability.Storage (class DurableStorage, getDoState, putDoState)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Common (AccountId, AccountType, Denomination, Instant)
import Data.Common (Instant)
import Data.Contract.Document.Ledger (LedgerDocumentContract(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Safe.Coerce (coerce)


newtype LedgerDocument = LedgerDocument
  { name :: String
  , createdAt :: Instant
  }

instance encodeDataContractLedgerDocument :: EncodeDataContract LedgerDocumentContract LedgerDocument where
  toContract x = LedgerDocumentContractV1 <<< coerce $ x
instance decodeDataContractLedgerDocument :: DecodeDataContract LedgerDocumentContract LedgerDocument where
  fromContract (LedgerDocumentContractV1 d) = pure <<< coerce $ d

ledgerDocumentId :: String
ledgerDocumentId = "ledger"

getLedger :: ∀ m. (DurableStorage m) => m _
getLedger = do
  LedgerDocument x <- getDoState "ledger"
  pure x

putLedger :: ∀ m. (DurableStorage m) => _ -> m Unit
putLedger x = putDoState ledgerDocumentId <<< LedgerDocument $ x

newtype AccountDocument = AccountDocument
  { accountId :: AccountId
  , name :: String
  , accountType :: AccountType
  , denomination :: Denomination
  , balance :: Int
  , closed :: Boolean
  }
derive instance genericAccountDocument :: Generic AccountDocument _
instance encodeJsonAccountDocument :: EncodeJson AccountDocument where
  encodeJson a = genericEncodeJson a
instance decodeJsonAccountDocument :: DecodeJson AccountDocument where
  decodeJson a = genericDecodeJson a
instance encodeDataContractAccountDocument :: EncodeDataContract AccountDocument AccountDocument where
  toContract = identity
instance decodeDataContractAccountDocument :: DecodeDataContract AccountDocument AccountDocument where
  fromContract = Just

getAccount :: ∀ m. (DurableStorage m) => AccountId -> m _
getAccount id = do
  AccountDocument x <- getDoState <<< toDocumentId $ id
  pure x

putAccount :: ∀ m. (DurableStorage m) => AccountId -> _ -> m Unit
putAccount id x = putDoState (toDocumentId id) <<< AccountDocument $ x