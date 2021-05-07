{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ribosome.Test.Orphans where

-- import Control.Monad.IO.Unlift (withRunInIO)
-- import Neovim (Neovim)

-- import Ribosome.Control.Monad.Ribo (Ribo(Ribo, unRibo))

-- instance AssertM (Neovim e) where
--   genericAssertFailure__ l =
--     liftIO . genericAssertFailure__ l

--   genericSubAssert l msg ma =
--     withRunInIO (\f -> genericSubAssert l msg (f ma))

-- instance AssertM (ExceptT e (Neovim s)) where
--   genericAssertFailure__ l = liftIO . genericAssertFailure__ l

--   genericSubAssert l msg =
--     ExceptT . genericSubAssert l msg . runExceptT

-- instance AssertM (Ribo s e) where
--   genericAssertFailure__ l = liftIO . genericAssertFailure__ l

--   genericSubAssert l msg =
--     Ribo . ExceptT . genericSubAssert l msg . runExceptT . unRibo
