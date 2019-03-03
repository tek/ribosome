{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Nvim.Api.Data where

import Data.ByteString.UTF8 (fromString)
import Data.MessagePack (Object)
import Neovim (Buffer, Tabpage, Window)
import Neovim.Classes (toObject)
import Prelude

import Ribosome.Nvim.Api.GenerateData (generateData)

generateData
