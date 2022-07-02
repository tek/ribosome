module Ribosome.Errors (
  module Ribosome.Errors,
  module Ribosome.Host.Effect.Errors,
  module Ribosome.Host.Interpreter.Errors,
) where

import Log (dataLog)

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.HandlerError (
  HandlerError,
  HandlerTag (GlobalTag),
  ToErrorMessage,
  resumeHandlerError,
  toHandlerError,
  )
import Ribosome.Host.Data.HostError (HostError (HostError))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Errors
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Interpreter.Errors

storeError ::
  ∀ e r .
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
  ∀ e r .
  ToErrorMessage e =>
  Member (DataLog HostError) r =>
  Maybe HandlerTag ->
  e ->
  Sem r ()
reportError htag e =
  withFrozenCallStack do
    storeError True htag e

reportStop ::
  ∀ e r .
  ToErrorMessage e =>
  Member (DataLog HostError) r =>
  Maybe HandlerTag ->
  Sem (Stop e : r) () ->
  Sem r ()
reportStop t sem =
  withFrozenCallStack do
    either (reportError t) pure =<< runStop sem

resumeReportError ::
  ∀ eff e r .
  ToErrorMessage e =>
  Members [eff !! e, DataLog HostError] r =>
  Maybe HandlerTag ->
  Sem (eff : r) () ->
  Sem r ()
resumeReportError t sem =
  withFrozenCallStack do
    sem !! reportError t

pluginHandlerErrors ::
  Members [Scratch !! RpcError, Settings !! SettingError, Rpc !! RpcError, Stop HandlerError] r =>
  InterpretersFor [Scratch, Settings, Rpc] r
pluginHandlerErrors =
  resumeHandlerError @Rpc .
  resumeHandlerError @Settings .
  resumeHandlerError @Scratch
