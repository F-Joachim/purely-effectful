{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module EffectfulDemo (runProgramWrappedWithTracer) where

import           Control.Exception                  (bracket)
import           Data.Text                          (Text)
import qualified Database.Redis                     as R
import           Effectful                          (Eff, runEff, (:>))
import           Effects.Cache                      (Cache (GetCached, SetCached), setCached, getCached)
import           Effects.Database                   (Database (FetchUserFromDB), fetchUserFromDB)
import           Effects.Logger                     (LogLevel (Error', Info, Warn),
                                                     Logger (LogMsg), log)
import           Effects.Tracing                    (Tracing, inSpan)
import           Interpreters.Cache.Pure            (runCachePure)
import           Interpreters.Cache.Redis           (runCacheRedis)
import           Interpreters.Db.Simulated          (runDatabaseSim)
import           Interpreters.Logger.Console        (runLoggerConsole)
import           Interpreters.Tracing.OpenTelemetry (runTracing)
import           OpenTelemetry.Exporter.OTLP.Span   (loadExporterEnvironmentVariables,
                                                     otlpExporter)
import           OpenTelemetry.Processor.Simple     (SimpleProcessorConfig (SimpleProcessorConfig),
                                                     simpleProcessor)
import           OpenTelemetry.Trace                (initializeGlobalTracerProvider)
import           OpenTelemetry.Trace.Core           (Tracer,
                                                     createTracerProvider,
                                                     defaultSpanArguments,
                                                     emptyTracerProviderOptions,
                                                     getGlobalTracerProvider,
                                                     getTracer, makeTracer,
                                                     setGlobalTracerProvider,
                                                     shutdownTracerProvider,
                                                     tracerOptions)
import           Prelude                       hiding (log)



-- | THE BUSINESS LOGIC
-- This function is completely decoupled from the implementation details.
-- It requires the 'Cache' effect and 'IOE' (the ability to run IO).
fetchUserData :: (Cache :> es, Database :> es, Logger :> es, Tracing :> es) => Text -> Eff es ()
fetchUserData userId = do
  inSpan "fetchUserData" defaultSpanArguments $ \_span -> do
    log Info ("Processing request for: " <> userId)

    maybeCached <- getCached userId
    case maybeCached of
      Just val ->
        log Info ("Cache HIT: " <> val)

      Nothing -> do
        log Warn "Cache MISS. Querying Database..."
        maybeDbVal <- fetchUserFromDB userId

        case maybeDbVal of
          Nothing ->
            log Error' "User not found in DB."
          Just dbVal -> do
            setCached userId dbVal
            log Info "Cache updated successfully."

runProgram :: Tracer -> IO ()
runProgram tracer = do
  putStrLn "--- RUNNING WITH MOCK INTERPRETER ---"

  runEff
    . runTracing tracer
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
        . runTracing tracer
        . runLoggerConsole
        . runDatabaseSim
        . runCacheRedis conn $ do
            fetchUserData "Bob"
            fetchUserData "Bob"

runProgramWrappedWithTracer :: IO ()
runProgramWrappedWithTracer = withTracer $ \tracer -> do
  runProgram tracer
  pure ()
  where
    withTracer f = bracket
      -- Install the SDK, pulling configuration from the environment
      initializeGlobalTracerProvider
      -- Ensure that any spans that haven't been exported yet are flushed
      shutdownTracerProvider
      (\tracerProvider -> do
        -- Get a tracer so you can create spans
        tp <- getGlobalTracerProvider
        let tracer = makeTracer tp "purely-effectful-app" tracerOptions
        f tracer
      )

-- Helper to safely try connecting to Redis for the demo
tryConnect :: IO (Either R.Reply R.Connection)
tryConnect = do
  conn <- R.connect R.defaultConnectInfo
  R.runRedis conn R.ping -- Ping to check connection
  return (Right conn)
