module Test.TestM where

import Prelude

import Capability.Fetch (class MonadFetchRequest)
import Capability.Has (class HasGetter, getter)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Argonaut (Json, stringify)
import Data.Fetch (RequestMethod(..))
import Data.Map (Map, empty)
import Data.Tuple (fst)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Safe.Coerce (coerce)

newtype TestM a = TestM (StateT TestData Aff a)

runTestM :: ∀ a. TestData -> TestM a -> Aff a
runTestM state = map fst <<< (runStateT <@> state) <<< coerce

derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM
derive newtype instance monadThrowTestM :: MonadThrow Error TestM
derive newtype instance monadErrorTestM :: MonadError Error TestM
derive newtype instance monadEffectTestM :: MonadEffect TestM
derive newtype instance monadAffTestM :: MonadAff TestM
derive newtype instance monadAskTestM :: MonadState TestData TestM

instance MonadFetchRequest TestM where
  getRequestMethod = gets getter
  getBodyString = gets getter

data TestData = TestData
  { requestMethod :: RequestMethod
  , requestBody :: String
  , data :: Map String Json
  }

mkTestData ∷ TestData
mkTestData = TestData
  { requestMethod: POST
  , requestBody: ""
  , data: empty
  }

setRequest
  :: ∀ m
   . MonadState TestData m
  => RequestMethod
  -> Json
  -> m Unit
setRequest method body = modify_ (\(TestData t) -> TestData t { requestBody = stringify body, requestMethod = method })

instance HasGetter RequestMethod TestData where
  getter (TestData x) = x.requestMethod

instance HasGetter String TestData where
  getter (TestData x) = x.requestBody
