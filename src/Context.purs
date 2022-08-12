module Context where

import Capability.Has (class Has, class HasSetter)
import Data.List (List(..))
import FFI.DurableObject (BatchedPut, DurableObjectRequest, DurableObjectState)

data ContextData = ContextData
  { durableObjectRequest :: DurableObjectRequest
  , durableObjectState :: DurableObjectState
  , batchPuts :: List BatchedPut
  , batchDeletes :: List String
  }

mkContext :: DurableObjectState -> DurableObjectRequest -> ContextData
mkContext durableObjectState durableObjectRequest =
  ContextData
    { durableObjectRequest
    , durableObjectState
    , batchPuts: Nil
    , batchDeletes: Nil
    }

instance hasContextState :: Has ContextData DurableObjectState where
  getter (ContextData x) = x.durableObjectState

instance hasContextRequest :: Has ContextData DurableObjectRequest where
  getter (ContextData x) = x.durableObjectRequest

instance hasContextBatchPutsGetter :: Has ContextData (List BatchedPut) where
  getter (ContextData x) = x.batchPuts

instance hasContextBatchPutsSetter :: HasSetter ContextData (List BatchedPut) where
  setter fn (ContextData x) = ContextData x { batchPuts = fn x.batchPuts }

instance hasContextBatchDeletesGetter :: Has ContextData (List String) where
  getter (ContextData x) = x.batchDeletes

instance hasContextBatchDeletesSetter :: HasSetter ContextData (List String) where
  setter fn (ContextData x) = ContextData x { batchDeletes = fn x.batchDeletes }






