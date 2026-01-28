{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Cache (Cache(GetCached, SetCached), getCached, setCached) where

import           Data.Text                  (Text)
import           Effectful                  (Dispatch (Dynamic), DispatchOf,
                                             Eff, Effect, (:>))
import           Effectful.Dispatch.Dynamic (send)

-- | The Cache effect definition.
data Cache :: Effect where
  GetCached :: Text -> Cache m (Maybe Text)
  SetCached :: Text -> Text -> Cache m ()

-- | Type family for dispatching the Cache effect.
type instance DispatchOf Cache = Dynamic

-- | Helper function to get a cached value.
getCached :: (Cache :> es) => Text -> Eff es (Maybe Text)
getCached key = send $ GetCached key

-- | Helper function to set a cached value.
setCached :: (Cache :> es) => Text -> Text -> Eff es ()
setCached key value = send $ SetCached key value