-- |Data types for Neovim mappings
module Ribosome.Data.Mapping where

import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.RpcName (RpcName)

-- |This ID type is intended to carry information about what buffer or other component triggered a mapping, if needed.
newtype MappingId =
  MappingId { unMappingId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode)

-- |All possible variants of Neovim's @map@ commands, causing mappings to be registered for different modes.
data MapMode =
  -- |@:map@ – normal, visual, select and operator-pending
  MapDefault
  |
  -- |@:nmap@ – normal
  MapNormal
  |
  -- |@:map!@ – insert and cmdline
  MapInsertCmdline
  |
  -- |@:imap@ – insert
  MapInsert
  |
  -- |@:cmap@ – cmdline
  MapCmdline
  |
  -- |@:lmap@ – insert, cmdline, lang-arg
  MapLangArg
  |
  -- |@:xmap@ – visual
  MapVisual
  |
  -- |@:smap@ – select
  MapSelect
  |
  -- |@:vmap@ – visual and select
  MapVisualSelect
  |
  -- |@:omap@ – operator-pending
  MapOperator
  deriving stock (Eq, Show)

-- |The character representing a map mode prefixing a @map@ command.
mapModePrefix :: MapMode -> Text
mapModePrefix = \case
  MapDefault -> ""
  MapNormal -> "n"
  MapInsertCmdline -> ""
  MapInsert -> "i"
  MapCmdline -> "c"
  MapLangArg -> "l"
  MapVisual -> "x"
  MapSelect -> "s"
  MapVisualSelect -> "v"
  MapOperator -> "o"

-- |The bang suffixing the insert+cmdline map cmd.
mapModeSuffix :: MapMode -> Text
mapModeSuffix = \case
  MapInsertCmdline -> "!"
  _ -> ""

noremapCmd :: MapMode -> Text
noremapCmd mm =
  [exon|#{mapModePrefix mm}noremap#{mapModeSuffix mm}|]

-- |This type associates a sequence of keys and a mode for a Neovim mapping with an RPC handler.
-- It is intended to be used with 'Ribosome.mappingFor' and 'Ribosome.activateBufferMapping'.
data Mapping =
  Mapping {
    -- |The name of the 'Ribosome.RpcHandler' that should be called when the mapping is triggered.
    rpc :: RpcName,
    -- |The key sequence that triggers the mapping, used with 'noremap'.
    lhs :: Text,
    -- |The modes in which the mapping should be installed.
    mode :: NonEmpty MapMode,
    -- |An optional string identifying the source of the mapping, for example when adding it to multiple buffers.
    id :: Maybe MappingId
  }
  deriving stock (Eq, Show)
