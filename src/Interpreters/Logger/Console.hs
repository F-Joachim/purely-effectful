{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Interpreters.Logger.Console where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Text                  as T (unpack)
import           Effectful                  (Eff, IOE, (:>))
import           Effectful.Dispatch.Dynamic (interpret)
import           Effects.Logger             (Logger (LogMsg))

runLoggerConsole :: (IOE :> es) => Eff (Logger ': es) a -> Eff es a
runLoggerConsole = interpret $ \_ -> \case
  LogMsg level msg -> (liftIO . putStrLn $ ("[" <> (show level <> ("] " <> T.unpack msg))))
