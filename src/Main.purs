module Main where

import Prelude

import AppM (AppM, runAppM, mkContext)
import Capability.Fetch (getBodyJson, getRequestMethod)
import Capability.Storage.Transactional (batchOperation)
import Control.Monad.Error.Class (catchError)
import Control.Promise (Promise, fromAff)
import Data.Fetch (RequestMethod(..), Response, errorResponse, messageResponse, notFoundResponse, toDurableObjectResponse)
import Effect (Effect)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)
import Handlers.Ledger (handleCommand)

toResponse :: AppM Response -> AppM DurableObjectResponse
toResponse x = toDurableObjectResponse <$> catchError x (pure <<< errorResponse)

ledgerFetchMain :: DurableObjectState -> DurableObjectRequest -> Effect (Promise DurableObjectResponse)
ledgerFetchMain state req = fromAff $ runAppM (mkContext state req) $ toResponse do
  getRequestMethod >>= case _ of
    PUT -> do
      cmd <- getBodyJson
      batchOperation $ handleCommand cmd
      pure $ messageResponse 200 "OK"
      -- TODO; schedule an alarm
    GET -> pure $ notFoundResponse "Query not yet implemented"
    _ -> pure $ notFoundResponse "not found"