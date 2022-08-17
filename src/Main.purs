module Main where

import Prelude

-- import AppM (runAppM, mkContext)
-- import Control.Promise (Promise, fromAff)
-- import Data.Response (toDurableObjectResponse)
-- import Effect (Effect)
-- import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState)
-- import Handlers.DurableObject (handleFetch)
-- import Handlers.Ledger (handleCommand, handleQuery)

-- ledgerFetchMain :: DurableObjectState -> DurableObjectRequest -> Effect (Promise DurableObjectResponse)
-- ledgerFetchMain state req = fromAff $ runAppM (mkContext state req) $ toDurableObjectResponse <$> (handleFetch handleCommand handleQuery)
