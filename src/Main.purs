module Main where

import Prelude

import AppM (runAppM, mkContext)
import Control.Promise (Promise, fromAff)
import Effect (Effect)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)
import Handlers.Ledger (handleLedgerRequest)

ledgerFetchMain :: DurableObjectState -> DurableObjectRequest -> Effect (Promise DurableObjectResponse)
ledgerFetchMain state req = fromAff $ runAppM (mkContext state req) handleLedgerRequest
