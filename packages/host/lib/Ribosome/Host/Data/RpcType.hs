module Ribosome.Host.Data.RpcType where

import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.RpcName (RpcName (RpcName), unRpcName)

newtype AutocmdEvents =
  AutocmdEvents { unAutocmdEvent :: [Text] }
  deriving stock (Eq, Show, Generic)
  deriving newtype (MsgpackEncode, MsgpackDecode)

instance IsString AutocmdEvents where
  fromString =
    AutocmdEvents . pure . fromString

newtype AutocmdPatterns =
  AutocmdPatterns { unAutocmdPattern :: [Text] }
  deriving stock (Eq, Show)
  deriving newtype (MsgpackEncode, MsgpackDecode)

instance IsString AutocmdPatterns where
  fromString =
    AutocmdPatterns . pure . fromString

newtype AutocmdBuffer =
  AutocmdBuffer { unAutocmdBuffer :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord, MsgpackEncode, MsgpackDecode)

instance Default AutocmdPatterns where
  def =
    "*"

newtype AutocmdGroup =
  AutocmdGroup { unAutocmdGroup :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, MsgpackEncode, MsgpackDecode)

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

newtype AutocmdId =
  AutocmdId { unAutocmdId :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord, MsgpackDecode, MsgpackEncode)

data CompleteStyle =
  CompleteFiltered
  |
  CompleteUnfiltered
  deriving stock (Eq, Show)

data CommandCompletion =
  CompleteBuiltin Text
  |
  CompleteHandler CompleteStyle RpcName
  deriving stock (Eq, Show)

completionName ::
  RpcName ->
  RpcName
completionName (RpcName n) =
  RpcName [exon|Complete_#{n}|]

completionValue :: CommandCompletion -> Text
completionValue = \case
  CompleteBuiltin completer ->
    [exon|#{completer}|]
  CompleteHandler CompleteFiltered func ->
    [exon|customlist,#{unRpcName (completionName func)}|]
  CompleteHandler CompleteUnfiltered func ->
    [exon|custom,#{unRpcName (completionName func)}|]

completionOption :: CommandCompletion -> Text
completionOption cc =
  [exon|-complete=#{completionValue cc}|]

data CommandOptions =
  CommandOptions {
    basic :: Map Text Object,
    completion :: Maybe CommandCompletion
  }
  deriving stock (Show)

newtype CommandArgs =
  CommandArgs { unCommandArgs :: [Text] }
  deriving stock (Eq, Show)

data RpcType =
  Function
  |
  Command CommandOptions CommandArgs
  |
  Autocmd AutocmdEvents AutocmdOptions
  deriving stock (Show, Generic)

methodPrefix :: RpcType -> Text
methodPrefix = \case
  Function -> "function"
  Command _ _ -> "command"
  Autocmd _ _ -> "autocmd"
