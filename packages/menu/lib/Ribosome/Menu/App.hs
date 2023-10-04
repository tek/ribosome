module Ribosome.Menu.App where

import qualified Data.Map.Strict as Map

import Ribosome.Data.Mapping (MapMode (MapInsert, MapNormal), MappingLhs, MappingSpec (MappingSpec))
import Ribosome.Menu.Action (
  MenuSem,
  MenuWidget,
  menuCycleFilter,
  menuDown,
  menuEsc,
  menuOk,
  menuQuit,
  menuToggle,
  menuToggleAll,
  menuUp,
  )
import Ribosome.Menu.Class.MenuState (MenuState)
import qualified Ribosome.Menu.Data.InputParams
import Ribosome.Menu.Data.InputParams (
  InputDomain (InputDomain),
  InputMode (InputMode),
  InputParams (InputParams),
  InputTrigger (InputMapping, InputPrompt),
  )
import Ribosome.Menu.Prompt.Data.Prompt (PromptControl (PromptControlApp, PromptControlItems), PromptState)

data AppTrigger =
  AppMapping MappingLhs InputDomain
  |
  AppPrompt PromptControl
  deriving stock (Eq, Show, Ord, Generic)

instance IsString AppTrigger where
  fromString key = AppMapping (fromString key) (InputDomain {modes = [MapNormal], prompt = Nothing})

type InputDispatch s r result =
  InputParams -> Maybe (MenuWidget s r result)

domain :: Traversal' AppTrigger InputDomain
domain = #_AppMapping . _2

modesLens :: Traversal' AppTrigger (NonEmpty MapMode)
modesLens = domain . #modes

modifyModes :: (NonEmpty MapMode -> NonEmpty MapMode) -> AppTrigger -> AppTrigger
modifyModes f = modesLens %~ f

withModes :: NonEmpty MapMode -> AppTrigger -> AppTrigger
withModes = modifyModes . (<>)

withMode :: MapMode -> AppTrigger -> AppTrigger
withMode = withModes . pure

withInsert :: AppTrigger -> AppTrigger
withInsert = withMode MapInsert

insert :: AppTrigger -> AppTrigger
insert = modesLens .~ [MapInsert]

type MenuApp s r result =
  Map AppTrigger (MenuWidget s r result)

inputMatrix :: AppTrigger -> MenuWidget s r result -> [(InputParams, MenuWidget s r result)]
inputMatrix (AppMapping lhs InputDomain {..}) widget =
  [
    (InputParams {trigger = InputMapping lhs, mode = InputMode {mode = m, prompt = p}}, widget)
    | m <- toList modes
    , p <- controls prompt
  ]
  where
    controls = \case
      Just p -> [p]
      Nothing -> [PromptControlApp, PromptControlItems]
inputMatrix (AppPrompt control) widget =
  [
    (InputParams {trigger = InputPrompt, mode = InputMode {mode = m, prompt = control}}, widget)
    | m <- [MapNormal, MapInsert]
  ]

appDispatch :: MenuApp s r result -> InputDispatch s r result
appDispatch app =
  flip Map.lookup matrix
  where
    matrix = Map.fromList (uncurry inputMatrix =<< Map.toList app)

appMappings :: MenuApp s r result -> [MappingSpec]
appMappings app =
  flip mapMaybe (Map.keys app) \case
    AppMapping lhs InputDomain {modes} -> Just (MappingSpec lhs modes)
    AppPrompt _ -> Nothing

builtinHandlers :: MenuApp s r result
builtinHandlers =
  [
    (withInsert "<esc>", menuEsc),
    (withInsert "<c-c>", menuQuit),
    (AppPrompt PromptControlItems, menuOk)
  ]

defaultHandlers ::
  MenuState s =>
  MenuApp s r result
defaultHandlers =
  [
    ("k", menuUp),
    (withInsert "<c-k>", menuUp),
    ("j", menuDown),
    (withInsert "<c-j>", menuDown),
    ("<space>", menuToggle),
    ("*", menuToggleAll),
    (withInsert "<m-f>", menuCycleFilter)
  ]

promptControl :: MenuWidget s r result -> MenuApp s r result -> MenuApp s r result
promptControl widget =
  Map.insert (AppPrompt PromptControlApp) widget

data PromptApp s r result =
  PromptApp {
    mappings :: [MappingSpec],
    dispatch :: InputDispatch s r result,
    prompt :: PromptState
  }
  deriving stock (Generic)

hoistPromptApp ::
  ∀ r r' s result .
  (∀ x . MenuSem s r x -> MenuSem s r' x) ->
  PromptApp s r result ->
  PromptApp s r' result
hoistPromptApp f PromptApp {..} =
  PromptApp {dispatch = fmap f . dispatch, ..}

promptApp ::
  ∀ s r result .
  MenuState s =>
  Bool ->
  Bool ->
  PromptState ->
  MenuApp s r result ->
  PromptApp s r result
promptApp addBuiltin addDefault prompt app =
  PromptApp (appMappings fullApp) (appDispatch fullApp) prompt
  where
    fullApp = handlersIf addBuiltin builtinHandlers <> handlersIf addDefault defaultHandlers <> app
    handlersIf flag hs | flag = hs
                       | otherwise = []
