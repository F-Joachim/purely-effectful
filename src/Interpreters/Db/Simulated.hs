{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Interpreters.Db.Simulated where

import           Control.Concurrent         (threadDelay)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text, pack)
import           Effectful                  (Eff, IOE, (:>))
import           Effectful.Dispatch.Dynamic (interpret)
import           Effects.Database           (Database (..))

-- Simulated database interactions for the Database effect
runDatabaseSim :: (IOE :> es) => Eff (Database : es) a -> Eff es a
runDatabaseSim = interpret $ \_ -> \case
  FetchUserFromDB userId -> do
    liftIO $ putStrLn "  (DB: SELECT * FROM users WHERE id = ...)"
    liftIO $ threadDelay 500000 -- Simulate 500ms latency
    -- Simulate that user "Ghost" does not exist
    if userId == pack "Ghost"
      then return Nothing
      else return $ Just $ pack "DB_Data_" <> userId
