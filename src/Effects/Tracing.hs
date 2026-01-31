{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Tracing
  ( Tracing (..),
    inSpan
  )
where

import           Data.Kind                  (Type)
import           Data.Text                  (Text)
import           Effectful                  (Dispatch (Dynamic), DispatchOf,
                                             Eff, Effect, type (:>))
import           Effectful.Dispatch.Dynamic (HasCallStack, send)
import           OpenTelemetry.Trace.Core   (Span, SpanArguments)

-- | The Tracing effect definition
type Tracing :: (Type -> Type) -> Type -> Type
data Tracing :: Effect where
  InSpan :: Text -> SpanArguments -> (Span -> m a) -> Tracing m a

type instance DispatchOf Tracing = 'Dynamic

-- | The effectful operation to create a new span
inSpan :: (HasCallStack, Tracing :> es) => Text -> SpanArguments -> (Span -> Eff es a) -> Eff es a
inSpan name args = send . InSpan name args
