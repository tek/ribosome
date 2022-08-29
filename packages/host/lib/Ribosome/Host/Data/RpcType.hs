module Ribosome.Host.Data.RpcType where

import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.RpcName (RpcName (RpcName), unRpcName)

-- |A set of autocmd event specifiers, like @BufEnter@, used to create and trigger autocmds.
newtype AutocmdEvents =
  AutocmdEvents { unAutocmdEvent :: [Text] }
  deriving stock (Eq, Show, Generic)
  deriving newtype (MsgpackEncode, MsgpackDecode)

instance IsString AutocmdEvents where
  fromString =
    AutocmdEvents . pure . fromString

-- |A file pattern like @*.hs@ that defines the files in which an autocmd should be triggered.
--
-- If the 'AutocmdEvents' contain @User@, this denotes the custom event name.
newtype AutocmdPatterns =
  AutocmdPatterns { unAutocmdPattern :: [Text] }
  deriving stock (Eq, Show)
  deriving newtype (MsgpackEncode, MsgpackDecode)

instance IsString AutocmdPatterns where
  fromString =
    AutocmdPatterns . pure . fromString

instance Default AutocmdPatterns where
  def =
    "*"

-- |The buffer number in which a buffer autocmd is supposed to be created.
newtype AutocmdBuffer =
  AutocmdBuffer { unAutocmdBuffer :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord, MsgpackEncode, MsgpackDecode)

-- |An autocmd group.
newtype AutocmdGroup =
  AutocmdGroup { unAutocmdGroup :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackEncode, MsgpackDecode)

-- |The options with which an autocmd may be defined.
--
-- See @:help :autocmd@.
data AutocmdOptions =
  AutocmdOptions {
    target :: Either AutocmdBuffer AutocmdPatterns,
    nested :: Bool,
    once :: Bool,
    group :: Maybe AutocmdGroup
  }
  deriving stock (Eq, Show, Generic)

instance Default AutocmdOptions where
  def =
    AutocmdOptions (Right "*") False False Nothing

instance IsString AutocmdOptions where
  fromString pat =
    def { target = Right (fromString pat) }

-- |Neovim assigns ID numbers to autocmds.
newtype AutocmdId =
  AutocmdId { unAutocmdId :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord, MsgpackDecode, MsgpackEncode)

-- |Neovim command completion can be designated as returning /all/ items that may be completed regardless of the current
-- word ('CompleteUnfiltered') or only those that match the current word ('CompleteFiltered').
data CompleteStyle =
  -- |Completion returns matching items.
  CompleteFiltered
  |
  -- |Completion returns all items.
  CompleteUnfiltered
  deriving stock (Eq, Show)

-- |The completion to use for a command.
data CommandCompletion =
  -- |Complete with one of the builtin completions, see @:help :command-completion@.
  CompleteBuiltin Text
  |
  -- |Complete with an RPC handler defined by a plugin.
  CompleteHandler CompleteStyle RpcName
  deriving stock (Eq, Show)

-- |Generate a name for the completion handler of a handler by prefixing its name with @Complete_@.
completionName ::
  RpcName ->
  RpcName
completionName (RpcName n) =
  RpcName [exon|Complete_#{n}|]

-- |Render a 'CommandCompletion' as the value to the @-complete=@ option for a command definition.
completionValue :: CommandCompletion -> Text
completionValue = \case
  CompleteBuiltin completer ->
    completer
  CompleteHandler CompleteFiltered func ->
    [exon|customlist,#{unRpcName (completionName func)}|]
  CompleteHandler CompleteUnfiltered func ->
    [exon|custom,#{unRpcName (completionName func)}|]

-- |Render a 'CommandCompletion' as the @-complete=@ option for a command definition.
completionOption :: CommandCompletion -> Text
completionOption cc =
  [exon|-complete=#{completionValue cc}|]

-- |Options for an RPC command on the Neovim side, consisting of the options described at @:help :command-attributes@
-- and an optional completion handler.
data CommandOptions =
  CommandOptions {
    basic :: Map Text Object,
    completion :: Maybe CommandCompletion
  }
  deriving stock (Show)

-- |The special arguments passed to an RPC call on the Neovim side that correspond to the declared 'CommandOptions'.
newtype CommandArgs =
  CommandArgs { unCommandArgs :: [Text] }
  deriving stock (Eq, Show)

-- |The type of RPC handler and its options.
data RpcType =
  Function
  |
  Command CommandOptions CommandArgs
  |
  Autocmd AutocmdEvents AutocmdOptions
  deriving stock (Show, Generic)

-- |The prefix for the method name used to identify an RPC handler.
methodPrefix :: RpcType -> Text
methodPrefix = \case
  Function -> "function"
  Command _ _ -> "command"
  Autocmd _ _ -> "autocmd"
