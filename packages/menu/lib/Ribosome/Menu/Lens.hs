module Ribosome.Menu.Lens where

import qualified Lens.Micro.Mtl as Lens

(#.) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ =
  coerce (\ x -> x :: b) :: forall a b. Coercible b a => a -> b

view ::
  Member (Reader s) r =>
  Getting a s a ->
  Sem r a
view l =
  asks (getConst #. l Const)
{-# inline view #-}

use ::
  Member (State s) r =>
  Getting a s a ->
  Sem r a
use l =
  gets (Lens.view l)
{-# inline use #-}

(%=) ::
  Member (State s) r =>
  ASetter s s a b ->
  (a -> b) ->
  Sem r ()
l %= f =
  modify' (l %~ f)
{-# inline (%=) #-}

infix 4 %=

(.=) ::
  Member (State s) r =>
  ASetter s s a b ->
  b ->
  Sem r ()
l .= b =
  modify' (l .~ b)
{-# inline (.=) #-}

(?=) ::
  Member (State s) r =>
  ASetter s s a (Maybe b) ->
  b ->
  Sem r ()
l ?= b =
  modify' (l ?~ b)
{-# inline (?=) #-}

infix 4 .=

(+=) ::
  Num a =>
  Member (State s) r =>
  ASetter s s a a ->
  a ->
  Sem r ()
l += a =
  l %= (+ a)
{-# inline (+=) #-}

infix 4 +=

(<.=) ::
  Member (State s) r =>
  ASetter s s a b ->
  b ->
  Sem r b
l <.= b = do
  l .= b
  pure b
{-# inline (<.=) #-}

infix 4 <.=
