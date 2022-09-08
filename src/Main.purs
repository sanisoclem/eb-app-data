module EB.DB.Main where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Promise (Promise, fromAff)
import EB.DB.AppM (AppM, runAppM, mkContext)
import EB.DB.Capability.Fetch (fromRequest, getRequestMethod)
import EB.DB.Capability.Storage.Transactional (batchOperation)
import EB.DB.Data.Fetch (RequestMethod(..), Response, errorResponse, jsonResponse, messageResponse, notFoundResponse, toDurableObjectResponse)
import EB.DB.FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)
import EB.DB.Handlers.Ledger (handleCommand, handleQuery)
import Effect (Effect)

toResponse :: AppM Response -> AppM DurableObjectResponse
toResponse x = toDurableObjectResponse <$> catchError x (pure <<< errorResponse)

ledgerFetchMain :: DurableObjectState -> DurableObjectRequest -> Effect (Promise DurableObjectResponse)
ledgerFetchMain state req = fromAff $ runAppM (mkContext state req) $ toResponse do
  getRequestMethod >>= case _ of
    PUT -> do
      cmd <- fromRequest
      batchOperation $ handleCommand cmd
      pure $ messageResponse 200 "OK"
      -- TODO; schedule an alarm
    GET -> do
      qry <- fromRequest
      result <- handleQuery qry
      pure $ jsonResponse 200 result
    _ -> pure $ notFoundResponse "not found"