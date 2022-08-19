module Main where

import Prelude

import AppM (runAppM, mkContext)
import Capability.Fetch (getBodyJson, getRequestMethod)
import Capability.Storage.Transactional (batchOperation)
import Control.Promise (Promise, fromAff)
import Data.Fetch (RequestMethod(..), messageResponse, notFoundResponse, toDurableObjectResponse)
import Effect (Effect)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)
import Handlers.Ledger (handleCommand)

ledgerFetchMain :: DurableObjectState -> DurableObjectRequest -> Effect (Promise DurableObjectResponse)
ledgerFetchMain state req = fromAff $ runAppM (mkContext state req) $ toDurableObjectResponse <$> do
  getRequestMethod >>= case _ of
    POST -> do
      cmd <- getBodyJson
      batchOperation $ handleCommand cmd
      pure $ messageResponse 200 "OK"
      -- TODO; schedule an alarm
    GET -> pure $ notFoundResponse "Query not yet implemented"
    _ -> pure $ notFoundResponse "not found"

