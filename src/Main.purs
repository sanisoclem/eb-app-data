module EbAppData.Main where

import Prelude

import EbAppData.AppM (AppM, runAppM, mkContext)
import Capability.Fetch (fromRequest, getRequestMethod)
import Capability.Storage.Transactional (batchOperation)
import Control.Monad.Error.Class (catchError)
import Control.Promise (Promise, fromAff)
import Data.Fetch (RequestMethod(..), Response, errorResponse, jsonResponse, messageResponse, notFoundResponse, toDurableObjectResponse)
import Effect (Effect)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)
import Handlers.Ledger (handleCommand, handleQuery)

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