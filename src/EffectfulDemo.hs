{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module EffectfulDemo (runProgram) where

import           Control.Concurrent           (threadDelay)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Map.Strict              as M
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Database.Redis               as R
import           Effectful                    (Dispatch (Dynamic), DispatchOf,
                                               Eff, Effect, IOE,
                                               MonadIO (liftIO), runEff,
                                               type (:>))
import           Effectful.Dispatch.Dynamic   (interpret, reinterpret, send)
import           Effectful.State.Static.Local (State, evalState, get, modify,
                                               runState)

-- | 1. THE EFFECT DEFINITION
-- |
-- --- Effect 1: Cache ---
-- We define a data type representing the operations available in our Cache.
data Cache :: Effect where
  GetCached :: Text -> Cache m (Maybe Text)
  SetCached :: Text -> Text -> Cache m ()

-- We choose 'Dynamic' dispatch so we can define multiple interpreters
-- (one for Redis, one for Testing).
type instance DispatchOf Cache = Dynamic

-- --- Effect 2: Database ---
data Database :: Effect where
  FetchUserFromDB :: Text -> Database m (Maybe Text)

type instance DispatchOf Database = Dynamic

-- --- Effect 3: Logger ---
data LogLevel = Info | Warn | Error' deriving (Show)

data Logger :: Effect where
  LogMsg :: LogLevel -> Text -> Logger m ()

type instance DispatchOf Logger = Dynamic

-- | 2. THE BUSINESS LOGIC
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

-- | 3. PRODUCTION INTERPRETER (Redis)
-- This function takes the abstract 'Cache' effect and removes it
-- by executing real Redis IO actions.
runCacheRedis :: (IOE :> es) => R.Connection -> Eff (Cache : es) a -> Eff es a
runCacheRedis conn = interpret $ \_ -> \case
  GetCached key -> liftIO $ R.runRedis conn $ do
    res <- R.get (TE.encodeUtf8 key)
    return $ case res of
      Right (Just val) -> Just (TE.decodeUtf8 val)
      _                -> Nothing

  SetCached key val -> liftIO $ R.runRedis conn $ do
    _ <- R.set (TE.encodeUtf8 key) (TE.encodeUtf8 val)
    return ()

-- | A "Production" DB interpreter that simulates a slow network call
runDatabaseSim :: (IOE :> es) => Eff (Database : es) a -> Eff es a
runDatabaseSim = interpret $ \_ -> \case
  FetchUserFromDB userId -> do
    liftIO $ putStrLn "  (DB: SELECT * FROM users WHERE id = ...)"
    liftIO $ threadDelay 500000 -- Simulate 500ms latency
    -- Simulate that user "Ghost" does not exist
    if userId == "Ghost"
      then return Nothing
      else return $ Just $ "DB_Data_" <> userId

runLoggerConsole :: (IOE :> es) => Eff (Logger : es) a -> Eff es a
runLoggerConsole = interpret $ \_ -> \case
  LogMsg level msg -> liftIO $ putStrLn $ "[" ++ show level ++ "] " ++ T.unpack msg

runLoggerNull :: Eff (Logger : es) a -> Eff es a
runLoggerNull = interpret $ \_ -> \case
  LogMsg _ _ -> return ()

-- | 4. TEST INTERPRETER (In-Memory Map)
-- Instead of IO, we handle the Cache effect using a State effect
-- carrying a simple Map.
runCachePure :: (IOE :> es) => Eff (Cache : es) a -> Eff es a
runCachePure = reinterpret (evalState @(M.Map Text Text) M.empty) $ \_ -> \case
  GetCached key -> do
    store <- get
    liftIO $ putStrLn $ "  (Mock Internal: Looking up " ++ T.unpack key ++ ")"
    return $ M.lookup key store

  SetCached key val -> do
    liftIO $ putStrLn $ "  (Mock Internal: Setting " ++ T.unpack key ++ " -> " ++ T.unpack val ++ ")"
    modify $ M.insert key val

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
