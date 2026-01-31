{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

module Effects.Logger (Logger(LogMsg), LogLevel (Error', Info, Warn), log) where

import           Data.Text                  (Text)
import           Effectful                  (Dispatch (Dynamic), DispatchOf,
                                             Eff, Effect, (:>))
import           Effectful.Dispatch.Dynamic (send)
import           Prelude                    hiding (log)
import Data.Kind (Type)

-- | The Logger effect definition
type Logger :: (Type -> Type) -> Type -> Type
data Logger :: Effect where
  LogMsg :: LogLevel -> Text -> Logger m ()

-- | Log levels for logging messages
type LogLevel :: Type
data LogLevel = Info | Warn | Error' deriving stock (Show)

-- | Type family for dispatching the Logger effect
type instance DispatchOf Logger = 'Dynamic

-- | Helper function to log messages
log :: (Logger :> es) => LogLevel -> Text -> Eff es ()
log level msg = send $ LogMsg level msg
