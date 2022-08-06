module Context where

import FFI.DurableObject (DurableObjectRequest, DurableObjectState)

type ContextData =
  { durableObjectRequest :: DurableObjectRequest
  , durableObjectState :: DurableObjectState
  }

mkContext :: DurableObjectState -> DurableObjectRequest -> ContextData
mkContext durableObjectState durableObjectRequest =
  { durableObjectRequest
  , durableObjectState
  }