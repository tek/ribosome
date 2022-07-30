module Ribosome.Handler (
  module Ribosome.Host.Data.RpcType,
  module Ribosome.Host.Handler,
) where

import Ribosome.Host.Data.RpcType (
  AutocmdEvents (..),
  AutocmdGroup (..),
  AutocmdOptions (..),
  AutocmdPatterns (..),
  CommandCompletion (..),
  CommandOptions (..),
  CompleteStyle (..),
  RpcType (..),
  )
import Ribosome.Host.Handler (
  complete,
  completeBuiltin,
  completeCustom,
  completeWith,
  )
