{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Interpreters.Tracing.OpenTelemetry (runTracing) where

import           Effectful                  (Eff, IOE,
                                             UnliftStrategy (SeqUnlift), (:>))
import           Effectful.Dispatch.Dynamic (interpret, localUnliftIO)
import           Effects.Tracing            (Tracing (..))
import           OpenTelemetry.Trace.Core   (Tracer, inSpan'')

runTracing :: forall es a. IOE :> es => Tracer -> Eff (Tracing ': es) a -> Eff es a
runTracing tracer = interpret $ \env -> \case
  InSpan name args f -> do
      localUnliftIO env SeqUnlift $ \unlift -> do
        inSpan'' tracer name args $ \span' -> unlift (f span')