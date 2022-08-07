module Ledger where

import Prelude

import AppM (AppM, runAppM)
import Capability.DurableObject (getRequestMethod, stringResponse)
import Context (mkContext)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Request (RequestMethod(..))
import Effect.Aff (Aff)
import FFI.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState, doErrorResponse)

fetchMain :: DurableObjectState -> DurableObjectRequest -> Aff DurableObjectResponse
fetchMain state req = either identity identity <$> lmap doErrorResponse <$> runAppM (mkContext state req) handler

handler :: AppM DurableObjectResponse
handler = do
  method <- getRequestMethod
  case method of
    POST -> handlerPost
    GET -> handlerGet
    PUT -> handlerPut
    _ -> stringResponse "Not found"


handlerPost :: AppM DurableObjectResponse
handlerPost = do
  stringResponse "POSTING"

handlerGet :: AppM DurableObjectResponse
handlerGet = do
  stringResponse "GetING"

handlerPut :: AppM DurableObjectResponse
handlerPut = do
  stringResponse "PutING"