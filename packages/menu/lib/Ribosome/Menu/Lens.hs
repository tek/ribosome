module Ribosome.Menu.Lens where

import Lens.Micro.Mtl (view)

use ::
  Member (State s) r =>
  Getting a s a ->
  Sem r a
use l =
  gets (view l)

(%=) ::
  Member (State s) r =>
  ASetter s s a b ->
  (a -> b) ->
  Sem r ()
l %= f =
  modify' (l %~ f)

infix 4 %=

(.=) ::
  Member (State s) r =>
  ASetter s s a b ->
  b ->
  Sem r ()
l .= b =
  modify' (l .~ b)

infix 4 .=

(+=) ::
  Num a =>
  Member (State s) r =>
  ASetter s s a a ->
  a ->
  Sem r ()
l += a =
  l %= (+ a)

infix 4 +=
