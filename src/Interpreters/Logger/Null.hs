{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Interpreters.Logger.Null where

import           Data.Text                  (Text)
import           Effectful                  (Eff, Effect)
import           Effectful.Dispatch.Dynamic (interpret)
import           Effects.Logger             (Logger (LogMsg))

runLoggerNull :: Eff (Logger : es) a -> Eff es a
runLoggerNull = interpret $ \_ -> \case
  LogMsg _ _ -> return ()
