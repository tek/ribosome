{-# options_haddock prune #-}

-- |Special command parameter that governs the range modifier.
module Ribosome.Host.Data.Range where

import Data.MessagePack (Object (ObjectArray))
import Exon (exon)

import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack, MsgpackDecode (fromMsgpack))

-- |Neovim offers different semantics for the command range (see @:help :command-range@).
--
-- This type determines the position (prefix line number/postfix count) and default values.
data RangeStyle =
  -- |Prefix line range, defaulting to the entire file (@-range=%@).
  RangeFile
  |
  -- |'Nothing': Prefix line range defaulting to the current line (@-range@).
  -- |@'Just' N@: Prefix count defaulting to @N@ (@-range=N@).
  RangeLine (Maybe Nat)
  |
  -- |@'Just' N@: Prefix or postfix count defaulting to @N@ (@-count=N@).
  -- |'Nothing': Same as @'Just' 0@ (@-count@).
  RangeCount (Maybe Nat)

-- |When this type is used as a parameter of a command handler function, the command is declared with the @-range@
-- option, and when invoked, the argument passed to the handler contains the line range specified by the user, as in:
--
-- > :5Reverse
-- > :5,20Reverse
--
-- In the first case, the field 'high' is 'Nothing'.
--
-- The type has a phantom parameter of kind 'RangeStyle' that configures the semantics of the range, as defined by
-- Neovim (see @:help :command-range@).
data Range (style :: RangeStyle) =
  Range {
    low :: Int64,
    high :: Maybe Int64
  }
  deriving stock (Eq, Show)

instance MsgpackDecode (Range style) where
  fromMsgpack = \case
    ObjectArray [Msgpack low, Msgpack high] ->
      Right (Range low (Just high))
    ObjectArray [Msgpack low] ->
      Right (Range low Nothing)
    o ->
      Left [exon|Range must be an array with one or two elements: #{show o}|]

class RangeStyleOpt (s :: RangeStyle) where
  rangeStyleOpt :: Text

  rangeStyleArg :: Text
  rangeStyleArg =
    "[<line1>, <line2>]"

instance RangeStyleOpt ('RangeLine 'Nothing) where
  rangeStyleOpt =
    "-range"

instance RangeStyleOpt 'RangeFile where
  rangeStyleOpt =
    "-range=%"

instance (
    KnownNat n
  ) => RangeStyleOpt ('RangeLine ('Just n)) where
  rangeStyleOpt =
    [exon|-range=#{show (natVal (Proxy @n))}|]

  rangeStyleArg =
    "[<count>]"

instance RangeStyleOpt ('RangeCount 'Nothing) where
  rangeStyleOpt =
    "-count"

  rangeStyleArg =
    "[<count>]"

instance (
    KnownNat n
  ) => RangeStyleOpt ('RangeCount ('Just n)) where
  rangeStyleOpt =
    [exon|-count=#{show (natVal (Proxy @n))}|]

  rangeStyleArg =
    "[<count>]"
