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
  = GetLedger
  | UpdateLedger
    { name :: String
    , createdAt :: Instant
    }

instance dataContractLedgerRequest :: DataContract LedgerRequestContract LedgerRequest where
  fromContract GetLedgerV1 = GetLedger
  fromContract (UpdateLedgerV1 x) = UpdateLedger x
  toContract GetLedger = GetLedgerV1
  toContract (UpdateLedger x) = UpdateLedgerV1 x
