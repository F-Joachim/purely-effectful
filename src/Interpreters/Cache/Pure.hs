{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Interpreters.Cache.Pure (runCachePure) where

import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Map.Strict              as M
import           Data.Text                    as T (Text, unpack)
import           Effectful                    (Eff, IOE, (:>))
import           Effectful.Dispatch.Dynamic   (reinterpret)
import           Effectful.State.Static.Local (evalState, get, modify)
import           Effects.Cache                (Cache (GetCached, SetCached))

runCachePure :: (IOE :> es) => Eff (Cache ': es) a -> Eff es a
runCachePure = reinterpret (evalState @(M.Map Text Text) M.empty) $ \_ -> \case
  GetCached key -> do
    store <- get
    liftIO . putStrLn $ "  (Mock Internal: Looking up " <> (T.unpack key <> ")")
    pure $ M.lookup key store

  SetCached key val -> do
    liftIO . putStrLn $ "  (Mock Internal: Setting " <> (T.unpack key <> (" -> " <> (T.unpack val <> ")")))
    modify $ M.insert key val
