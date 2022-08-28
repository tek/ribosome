-- |Data types for Neovim mappings
module Ribosome.Data.Mapping where

import Data.MessagePack (Object)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.Event (EventName)
import Ribosome.Host.Data.RpcName (RpcName)

-- |The sequence of keys that triggers a mapping.
newtype MappingLhs =
  MappingLhs { unMappingLhs :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

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
  deriving stock (Eq, Show, Ord, Enum)

instance Default MapMode where
  def =
    MapNormal

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

-- |The character representing a map mode when passing it to an api function.
mapModeShortName :: MapMode -> Text
mapModeShortName m =
  mapModePrefix m <> mapModeSuffix m

-- |The action that should be performed when a mapping is triggered.
data MappingAction =
  -- |The name of the 'Ribosome.RpcHandler' that should be called when the mapping is triggered.
  MappingCall RpcName
  |
  -- |The event to publish when the mapping is triggered.
  MappingEvent EventName
  deriving stock (Eq, Show)

-- |The configuration for a mapping that is specific to Neovim.
data MappingSpec =
  MappingSpec {
    -- |The key sequence that triggers the mapping.
    lhs :: MappingLhs,
    -- |The modes in which the mapping should be installed.
    mode :: NonEmpty MapMode
  }
  deriving stock (Eq, Show, Ord, Generic)

instance IsString MappingSpec where
  fromString s =
    MappingSpec (fromString s) [def]

-- |This type associates a sequence of keys and a mode for a Neovim mapping with an RPC handler or event.
-- It is intended to be used with 'Ribosome.mappingFor' or 'Ribosome.eventMapping' and 'Ribosome.activateBufferMapping'.
data Mapping =
  Mapping {
    -- |The action to take when the mapping is triggered.
    action :: MappingAction,
    -- |The Neovim related configuration for the mapping, i.e. its key sequence and modes.
    spec :: MappingSpec,
    -- |An optional string identifying the source of the mapping, for example when adding it to multiple buffers.
    id :: Maybe MappingId,
    -- |Options like @remap@, @nowait@ or @desc@.
    opts :: Map Text Object
  }
  deriving stock (Eq, Show, Generic)
