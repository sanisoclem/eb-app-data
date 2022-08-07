module Context where

import Capability.Has

import FFI.DurableObject (DurableObjectRequest, DurableObjectState)

data ContextData = ContextData
  { durableObjectRequest :: DurableObjectRequest
  , durableObjectState :: DurableObjectState
  }

mkContext :: DurableObjectState -> DurableObjectRequest -> ContextData
mkContext durableObjectState durableObjectRequest =
  ContextData
    { durableObjectRequest
    , durableObjectState
    }

instance hasContextState :: Has ContextData DurableObjectState where
  getter (ContextData x) = x.durableObjectState

instance hasContextRequest :: Has ContextData DurableObjectRequest where
  getter (ContextData x) = x.durableObjectRequest