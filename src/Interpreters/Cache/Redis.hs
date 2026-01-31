{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Interpreters.Cache.Redis (runCacheRedis) where

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Text.Encoding         as TE
import qualified Database.Redis             as R
import           Effectful                  (Eff, IOE, (:>))
import           Effectful.Dispatch.Dynamic (interpret)
import           Effects.Cache              (Cache (..))

runCacheRedis :: (IOE :> es) => R.Connection -> Eff (Cache ': es) a -> Eff es a
runCacheRedis conn = interpret $ \_ -> \case
  GetCached key -> (liftIO . R.runRedis conn $ (do
    res <- R.get (TE.encodeUtf8 key)
    pure $ case res of
      Right (Just val) -> Just (TE.decodeUtf8 val)
      _                -> Nothing))

  SetCached key val -> (liftIO . R.runRedis conn $ (do
    _ <- R.set (TE.encodeUtf8 key) (TE.encodeUtf8 val)
    pure ()))
