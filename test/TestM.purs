module Test.TestM where

import Prelude

import Capability.DataContract (class EncodeDataContract, decodeContractJson, encodeContractJson)
import Capability.Has (getter, setter, class Has, class HasSetter)
import Capability.IncomingRequest (class IncomingRequest)
import Capability.Storage (class DurableStorage)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, liftEither)
import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Argonaut (Json, stringify)
import Data.List (List(..), (:))
import Data.Map (Map, delete, empty, insert, lookup)
import Data.Request (RequestMethod(..))
import Data.Traversable (sequence, sequence_)
import Data.Tuple (fst)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
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

instance durableStorageTestM :: DurableStorage TestM where
  tryGetState key = do
    val <- lookup key <$> gets getter
    sequence $ liftEither <<< decodeContractJson <$> val
  putState key value = do
    modify_ <<< setter <<< insert key <<< encodeContractJson $ value
  deleteState key = modify_ <<< setter $ deleteFromJsonMap key

  batchPutState id doc = modify_ <<< setter <<< (:) <<< BatchedPut $ { id, document: encodeContractJson doc }
  batchDeleteState id = modify_ <<< setter <<< (:) $ id
  commitBatchState = do
    deletes :: List String <- gets getter
    puts :: List BatchedPut <- gets getter
    sequence_ $ modify_ <<< setter <<< deleteFromJsonMap <$> deletes
    sequence_ $ modify_ <<< setter <<< (\(k :: { id :: String, document :: Json }) -> insert k.id k.document) <<< coerce <$> puts
    modify_ <<< setter <<< const $ (Nil :: List String)
    modify_ <<< setter <<< const $ (Nil :: List BatchedPut)

deleteFromJsonMap :: String -> Map String Json -> Map String Json
deleteFromJsonMap = delete

instance incomingRequestTestM :: IncomingRequest TestM where
  getRequestMethod = gets getter
  getBodyString = gets getter

newtype BatchedPut = BatchedPut
  { id :: String
  , document :: Json
  }

data TestData = TestData
  { requestMethod :: RequestMethod
  , requestBody :: String
  , data :: Map String Json
  , batchPuts :: List BatchedPut
  , batchDeletes :: List String
  }
mkTestData ∷ TestData
mkTestData = TestData
  { requestMethod: POST
  , requestBody: ""
  , data: empty
  , batchPuts: Nil
  , batchDeletes: Nil
  }

setRequest
  :: ∀ m a b
   . MonadState TestData m
  => EncodeDataContract b a
  => a
  -> m Unit
setRequest x = modify_ (\(TestData t) -> TestData t { requestBody = stringify <<< encodeContractJson $ x })

instance hasContextMethod :: Has TestData RequestMethod where
  getter (TestData x) = x.requestMethod

instance hasContextBody :: Has TestData String where
  getter (TestData x) = x.requestBody

instance hasContextBatchPutsGetter :: Has TestData (List BatchedPut) where
  getter (TestData x) = x.batchPuts

instance hasContextBatchPutsSetter :: HasSetter TestData (List BatchedPut) where
  setter fn (TestData x) = TestData x { batchPuts = fn x.batchPuts }

instance hasContextBatchDeletesGetter :: Has TestData (List String) where
  getter (TestData x) = x.batchDeletes

instance hasContextBatchDeletesSetter :: HasSetter TestData (List String) where
  setter fn (TestData x) = TestData x { batchDeletes = fn x.batchDeletes }

instance hasContextDataGetter :: Has TestData (Map String Json) where
  getter (TestData x) = x.data

instance hasContextDataSetter :: HasSetter TestData (Map String Json) where
  setter fn (TestData x) = TestData x { data = fn x.data }


