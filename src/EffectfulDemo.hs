{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module EffectfulDemo (runProgram) where

import           Data.Text                       (Text)
import qualified Database.Redis                  as R
import           Effectful                       (Eff, runEff, (:>))
import           Effectful.Dispatch.Dynamic      (send)
import           Effects.Cache                   (Cache (GetCached, SetCached))
import           Effects.Database                (Database (FetchUserFromDB))
import           Effects.Logger                  (LogLevel (Error', Info, Warn),
                                                  Logger (LogMsg))
import           Interpreters.Cache.Pure         (runCachePure)
import           Interpreters.Cache.Redis        (runCacheRedis)
import           Interpreters.Database.Simulated (runDatabaseSim)
import           Interpreters.Logger.Console     (runLoggerConsole)


-- | THE BUSINESS LOGIC
-- This function is completely decoupled from the implementation details.
-- It requires the 'Cache' effect and 'IOE' (the ability to run IO).
fetchUserData :: (Cache :> es, Database :> es, Logger :> es) => Text -> Eff es ()
fetchUserData userId = do
  send $ LogMsg Info ("Processing request for: " <> userId)

  maybeCached <- send (GetCached userId)
  case maybeCached of
    Just val ->
      send $ LogMsg Info ("Cache HIT: " <> val)

    Nothing -> do
      send $ LogMsg Warn "Cache MISS. Querying Database..."
      maybeDbVal <- send (FetchUserFromDB userId)

      case maybeDbVal of
        Nothing ->
          send $ LogMsg Error' "User not found in DB."
        Just dbVal -> do
          send $ SetCached userId dbVal
          send $ LogMsg Info "Cache updated successfully."

runProgram :: IO ()
runProgram = do
  putStrLn "--- RUNNING WITH MOCK INTERPRETER ---"
  -- We use runEff to run the final stack.
  -- Notice we pick 'runCachePure' here.
  runEff
    . runLoggerConsole
    . runDatabaseSim
    . runCachePure $ do
        fetchUserData "Alice"
        fetchUserData "Alice" -- Should be a hit now
        fetchUserData "Ghost"

  putStrLn "\n--- RUNNING WITH REDIS INTERPRETER ---"
  putStrLn "(Ensure Redis is running on localhost:6379, or this will fail)"

  -- Simple check to see if we can connect, otherwise skip
  connEither <- tryConnect
  case connEither of
    Left _ -> putStrLn "Skipping Redis demo (Connection failed)"
    Right conn -> do
      runEff
        . runLoggerConsole
        . runDatabaseSim
        . runCacheRedis conn $ do
            fetchUserData "Bob"
            fetchUserData "Bob"

-- Helper to safely try connecting to Redis for the demo
tryConnect :: IO (Either R.Reply R.Connection)
tryConnect = do
  conn <- R.connect R.defaultConnectInfo
  R.runRedis conn R.ping -- Ping to check connection
  return (Right conn)
