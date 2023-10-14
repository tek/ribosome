{-# options_haddock prune #-}
{-# language NoOverloadedLists #-}

-- |Compiler for the syntax DSL algebra, internal.
module Ribosome.Syntax.Build where

import Data.Tuple.Extra (uncurry3)
import Polysemy (run)

import Ribosome.Data.Syntax.Dsl (Alg (..), Building (..))
import Ribosome.Data.Syntax.Syntax (HiLink (HiLink), Highlight (Highlight), Syntax (Syntax))
import Ribosome.Data.SyntaxItem (SyntaxGroup, SyntaxItem)

compileItem ::
  Member (Reader Building) r =>
  SyntaxItem ->
  Sem r SyntaxItem
compileItem i = do
  ask <&> \ Building {..} ->
    i
    & #contained .~ contained
    & #group %~ (prefix <>)
    & #next %~ (<> next)
    & #contains %~ (<> contains)

-- |Compile 'Alg'.
spin ::
  Members [Writer [SyntaxItem], Writer [(Map Text Text, [SyntaxGroup])], Writer [HiLink], Reader Building] r =>
  Alg ->
  Sem r [SyntaxGroup]
spin = \case
  Item i -> do
    compiled <- compileItem i
    [compiled ^. #group] <$ tell [compiled]
  Chain l r -> do
    -- @r@ should only ever match when requested by @l@.
    -- Set @contained@ to prevent @r@ from matching at top level.
    -- @contains@ propagates to both since there's no obvious interpretation of @(l >- r) #> i@.
    next <- local (#contained .~ True) (spin r)
    -- Set @next@ on @l@ to force @r@ afterwards.
    local (#next .~ next) (spin l)
  Choice l r ->
    -- Everything propagates to both alternatives.
    spin l <> spin r
  Contain l r -> do
    -- Set @contained@ to prevent @r@ from matching at top level.
    next <- local ((#contained .~ True) . (#next .~ [])) (spin r)
    -- Set @contains@ to allow @r@ in @l@.
    local (#contains .~ next) (spin l)
  Prefix pref s ->
    local (#prefix <>~ pref) (spin s)
  Hi vals s -> do
    next <- spin s
    tell [(vals, next)]
    pure next
  Link target s -> do
    next <- spin s
    tell (flip HiLink target <$> next)
    pure next
  Mod f s ->
    local (#modItem %~ \ m -> f . m) (spin s)

compileHi :: Map Text Text -> [SyntaxGroup] -> Maybe (Highlight, [HiLink])
compileHi vals = \case
  [] ->
    Nothing
  h : l ->
    Just (Highlight h vals, link <$> l)
    where
      link from =
        HiLink from h

compileHis :: [(Map Text Text, [SyntaxGroup])] -> ([Highlight], [HiLink])
compileHis =
  second join .
  unzip .
  mapMaybe (uncurry compileHi)

compile :: Alg -> ([SyntaxItem], [Highlight], [HiLink])
compile spec =
  (syn, hi, links <> extraLinks)
  where
    (hi, extraLinks) =
      compileHis hiAssoc
    (syn, (hiAssoc, links)) =
      run do
        runWriterAssocR $ runWriterAssocR do
          fst <$> runWriterAssocR (runReader def (spin spec))

-- |Compile a syntax declaration written in the DSL to a 'Syntax', which can be used with
-- 'Ribosome.Api.Syntax.executeWindowSyntax' to apply it to a Neovim window.
build :: Alg -> Syntax
build =
  uncurry3 Syntax . compile
