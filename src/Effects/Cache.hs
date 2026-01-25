{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Cache where

import           Data.Text (Text)
import           Effectful (Dispatch (Dynamic), DispatchOf, Effect)

-- | The Cache effect definition.
data Cache :: Effect where
  GetCached :: Text -> Cache m (Maybe Text)
  SetCached :: Text -> Text -> Cache m ()

-- | Type family for dispatching the Cache effect.
type instance DispatchOf Cache = Dynamic
