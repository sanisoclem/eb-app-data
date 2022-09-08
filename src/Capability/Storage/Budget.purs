module EB.DB.Capability.Storage.Budget where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe)
import EB.DB.Capability.Storage.Cf (class MonadCfStorage)
import EB.DB.Capability.Storage.Database (class MonadDatabase, class MonadReadonlyDatabase, putDocument, tryGetDocument, tryGetDocumentReadonly)
import EB.DB.Capability.Storage.Transactional (class MonadTransactionalStorage)
import EB.DB.Data.Database.Budget (BudgetDatabaseId, BudgetSummaryId(..), BudgetSummaryRecord, budgetSummaryDocument, unBudgetSummaryDocument)
import Effect.Exception (Error)

class Monad m <= MonadBudgetReadonlyDb m where
  getSummaryReadonly :: m (Maybe BudgetSummaryRecord)

class Monad m <= MonadBudgetDb m where
  getSummary :: m (Maybe BudgetSummaryRecord)
  putSummary :: BudgetSummaryRecord -> m Unit

instance (Monad m, MonadThrow Error m, MonadDatabase BudgetDatabaseId m,  MonadTransactionalStorage m) => MonadBudgetDb m where
  getSummary = map unBudgetSummaryDocument <$> tryGetDocument BudgetSummaryId
  putSummary = putDocument BudgetSummaryId <<< budgetSummaryDocument


instance (Monad m, MonadThrow Error m, MonadReadonlyDatabase BudgetDatabaseId m, MonadCfStorage m) => MonadBudgetReadonlyDb m where
  getSummaryReadonly = map unBudgetSummaryDocument <$> tryGetDocumentReadonly BudgetSummaryId
