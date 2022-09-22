module Test.TestM
  ( TestData
  , TestM
  , mkTestData
  , runTestM
  , testNow
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Argonaut (Json)
import Data.Argonaut.Core as JSON
import Data.Array (fromFoldable)
import Data.Lens (Iso', iso)
import Data.Lens.Record (prop)
import Data.Map (Map, delete, empty, filterWithKey, insert, lookup)
import Data.Maybe (fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (fst)
import EB.DB.Capability.Has (class HasLens, getState, modifyState_)
import EB.DB.Capability.Storage.Cf (class MonadCfStorage)
import EB.DB.Capability.Outbox (outboxDocumentId)
import EB.DB.Capability.Storage.Transactional (class MonadTransactionalStorage)
import EB.DB.Data.Instant (Instant, mkInstant)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))

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
derive newtype instance monadStateTestM :: MonadState TestData TestM

instance MonadTransactionalStorage TestM where
  batchtryGetDurableState key = lookup key <$> getState
  batchputDurableState k v = modifyState_ $ insert k v
  batchdeleteDurableState k = modifyState_ (\(m :: Map String Json) -> delete k m)

instance MonadCfStorage TestM where
  tryGetDurableState key = lookup key <$> getState
  putDurableState k v = modifyState_ $ insert k v
  deleteDurableState k = modifyState_ (\(m :: Map String Json) -> delete k m)
  getDurableStateByPrefix prefix = fromFoldable <$> filterWithKey (\k -> \_ -> isJust $ stripPrefix (Pattern prefix) k) <$> getState

data TestData = TestData
  { data :: Map String Json
  , nowUtc :: Instant
  }

testNow :: Instant
testNow = fromMaybe top $ mkInstant 735462.0

mkTestData ∷ TestData
mkTestData = TestData
  { data:
      insert outboxDocumentId (JSON.fromArray [])
        $ empty
  , nowUtc: testNow
  }

_TestData :: Iso' TestData { data :: Map String Json, nowUtc :: Instant }
_TestData = iso (case _ of TestData x -> x) TestData

instance HasLens TestData (Map String Json) where
  focus = _TestData <<< prop (Proxy :: Proxy "data")

instance HasLens TestData Instant where
  focus = _TestData <<< prop (Proxy :: Proxy "nowUtc")
