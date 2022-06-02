# Overview

_Ribosome_ is a suite of libraries for building [Neovim](https://neovim.io) remote plugins in Haskell, using the
algebraic effect system [Polysemy](https://hackage.haskell.org/package/polysemy) as its foundation.

Its components are:

* [ribosome](https://hackage.haskell.org/package/ribosome-host/docs/Ribosome.html) High-level functionality for writing
  plugins
* [ribosome-host](https://hackage.haskell.org/package/ribosome-host/docs/Ribosome-Host.html) MessagePack RPC server and
  TH-generated API functions
* [ribosome-host-test](https://hackage.haskell.org/package/ribosome-host/docs/Ribosome-Host-Test.html) Test utilities
  for `ribosome-host`
* [ribosome-test](https://hackage.haskell.org/package/ribosome-host/docs/Ribosome-Test.html) Test utilities for
  `ribosome`
* [ribosome-menu](https://hackage.haskell.org/package/ribosome-host/docs/Ribosome-Menu.html) A fuzzy-finder menu tool

