module Ribosome.Nvim.Api.IO where

import Data.MessagePack (Object)

import qualified Ribosome.Nvim.Api.Data as RpcData
import Ribosome.Nvim.Api.Data (Buffer, LuaRef, Tabpage, Window)
import Ribosome.Nvim.Api.GenerateIO (generateIO)

generateIO
