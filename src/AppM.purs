module AppM where

import Prelude

import Capability.Fetch (class MonadFetchRequest)
import Capability.Has (class HasGetter, getter)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, gets, runStateT)
import Data.Fetch (RequestMethod(..))
import Data.Tuple (fst)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import FFI.DurableObject (DurableObjectRequest, DurableObjectState, doRequestGetBody, doRequestGetMethod)
import Safe.Coerce (coerce)

newtype AppM a = AppM (StateT ContextData Aff a)

runAppM :: ∀ a. ContextData -> AppM a -> Aff a
runAppM state = map fst <<< (runStateT <@> state) <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadThrowAppM :: MonadThrow Error AppM
derive newtype instance monadErrorAppM :: MonadError Error AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadState ContextData AppM

instance incomingRequestAppM :: MonadFetchRequest AppM where
  getRequestMethod = do
    request <- gets getter
    pure $ case doRequestGetMethod request of
      "POST" -> POST
      "GET" -> GET
      "DELETE" -> DELETE
      "PUT" -> PUT
      x -> Unknown x

  getBodyString = do
    request <- gets getter
    liftAff <<< doRequestGetBody $ request

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

instance hasContextState :: HasGetter DurableObjectState ContextData  where
  getter (ContextData x) = x.durableObjectState

instance hasContextRequest :: HasGetter DurableObjectRequest ContextData  where
  getter (ContextData x) = x.durableObjectRequest
