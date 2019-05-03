{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ribosome.Test.Orphans where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Neovim (Neovim)
import Test.Framework.AssertM (AssertM(..))

import Ribosome.Control.Monad.Ribo (RNeovim, Ribo(Ribo, unRibo))

instance AssertM (Neovim e) where
  genericAssertFailure__ l =
    liftIO . genericAssertFailure__ l

  genericSubAssert l msg ma =
    withRunInIO (\f -> genericSubAssert l msg (f ma))

instance AssertM (ExceptT e (Neovim s)) where
  genericAssertFailure__ l = liftIO . genericAssertFailure__ l

  genericSubAssert l msg =
    ExceptT . genericSubAssert l msg . runExceptT

instance AssertM (Ribo s e) where
  genericAssertFailure__ l = liftIO . genericAssertFailure__ l

  genericSubAssert l msg =
    Ribo . ExceptT . genericSubAssert l msg . runExceptT . unRibo
