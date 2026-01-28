{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database (Database(FetchUserFromDB), fetchUserFromDB) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Effectful              (Dispatch (Dynamic), DispatchOf, Effect, Eff, (:>))
import Effectful.Dispatch.Dynamic (send)

-- | Effect for database operations
data Database :: Effect where
  FetchUserFromDB :: Text -> Database m (Maybe Text)

-- | Type family for dispatching the Database effect
type instance DispatchOf Database = Dynamic

-- | Helper function to fetch a user from the database
fetchUserFromDB :: (Database :> es) => Text -> Eff es (Maybe Text)
fetchUserFromDB userId = send $ FetchUserFromDB userId