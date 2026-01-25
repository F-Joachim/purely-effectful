{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Effectful              (Dispatch (Dynamic), DispatchOf, Effect)

-- | Effect for database operations
data Database :: Effect where
  FetchUserFromDB :: Text -> Database m (Maybe Text)

-- | Type family for dispatching the Database effect
type instance DispatchOf Database = Dynamic
