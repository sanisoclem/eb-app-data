module Test.TestM
  ( TestData
  , TestM
  , mkTestData
  , runTestM
  , testNow
  )
  where

import Prelude

import Capability.Has (class HasGetter, class HasSetter, getter, setter)
import Capability.Now (class MonadNow, nowUtc)
import Capability.Storage.Cf (class MonadCfStorage)
import Capability.Storage.Outbox (outboxDocumentId)
import Capability.Storage.Transactional (class MonadTransactionalStorage)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Argonaut (Json)
import Data.Argonaut.Core as JSON
import Data.Instant (Instant(..), mkInstant)
import Data.Map (Map, delete, empty, insert, lookup)
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
derive newtype instance monadStateTestM :: MonadState TestData TestM

instance MonadTransactionalStorage TestM where
  batchTryGetState key = lookup key <$> gets getter
  batchPutState k v = modify_ (setter $ insert k v)
  batchDeleteState k = modify_ $ setter (\(m :: Map String Json) -> delete k m)

instance MonadCfStorage TestM where
  tryGetState key = lookup key <$> gets getter
  putState k v = modify_ (setter $ insert k v)
  deleteState k = modify_ $ setter (\(m :: Map String Json) -> delete k m)

instance MonadNow TestM where
  nowUtc = gets getter

data TestData = TestData
  { data :: Map String Json
  , nowUtc :: Instant
  }

testNow :: Instant
testNow = mkInstant 735462

mkTestData ∷ TestData
mkTestData = TestData
  { data:
      insert outboxDocumentId (JSON.fromArray [])
        $ empty
  , nowUtc: testNow
  }

instance HasGetter (Map String Json) TestData where
  getter (TestData x) = x.data

instance HasGetter Instant TestData where
  getter (TestData x) = x.nowUtc

instance HasSetter (Map String Json) TestData where
  setter fn (TestData x) = TestData x { data = fn x.data }
