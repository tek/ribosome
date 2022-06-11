module Ribosome.Errors (
  module Ribosome.Errors,
  module Ribosome.Host.Effect.Errors,
) where

import GHC.Stack (withFrozenCallStack)
import Log (dataLog)

import Ribosome.Host.Data.HandlerError (HandlerTag (GlobalTag), ToErrorMessage, toHandlerError)
import Ribosome.Host.Data.HostError (HostError (HostError))
import Ribosome.Host.Effect.Errors

storeError ::
  ToErrorMessage e =>
  Member (DataLog HostError) r =>
  Bool ->
  Maybe HandlerTag ->
  e ->
  Sem r ()
storeError report htag (toHandlerError (fromMaybe GlobalTag htag) -> err) =
  withFrozenCallStack do
    dataLog (HostError report err)

reportError ::
  ToErrorMessage e =>
  Member (DataLog HostError) r =>
  Maybe HandlerTag ->
  e ->
  Sem r ()
reportError htag e =
  withFrozenCallStack do
    storeError True htag e
