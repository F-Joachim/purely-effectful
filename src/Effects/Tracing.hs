{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Effects.Tracing
  ( Tracing (..),
    inSpan
  )
where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Text                  (Text)
import           Effectful                  (Dispatch (Dynamic), DispatchOf,
                                             Eff, Effect, IOE, MonadIO (liftIO),
                                             MonadUnliftIO (withRunInIO),
                                             UnliftStrategy (SeqUnlift),
                                             type (:>))
import           Effectful.Dispatch.Dynamic (HasCallStack, interpret,
                                             localUnlift, localUnliftIO,
                                             reinterpret, send)
import           Effectful.Internal.Monad   (withEffToIO)
import           OpenTelemetry.Trace.Core   (Span, SpanArguments, Tracer,
                                             getTracer, inSpan'')

-- | The Tracing effect definition
data Tracing :: Effect where
  InSpan :: Text -> SpanArguments -> (Span -> m a) -> Tracing m a

type instance DispatchOf Tracing = Dynamic

-- | The effectful operation to create a new span
inSpan :: (HasCallStack, Tracing :> es) => Text -> SpanArguments -> (Span -> Eff es a) -> Eff es a
inSpan name args = send . InSpan name args
