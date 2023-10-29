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

newtype TriggerPrio =
  TriggerPrio Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data AppTrigger =
  AppMapping TriggerPrio MappingLhs InputDomain
  |
  AppPrompt TriggerPrio PromptControl
  deriving stock (Eq, Show, Ord, Generic)

userPrioValue :: TriggerPrio
userPrioValue = 5

instance IsString AppTrigger where
  fromString key = AppMapping userPrioValue (fromString key) (InputDomain {modes = [MapNormal], prompt = Nothing})

type InputDispatch s r result =
  InputParams -> Maybe (MenuWidget s r result)

domain :: Traversal' AppTrigger InputDomain
domain = #_AppMapping . _3

modesLens :: Traversal' AppTrigger (NonEmpty MapMode)
modesLens = domain . #modes

controlLens :: Traversal' AppTrigger (Maybe PromptControl)
controlLens = domain . #prompt

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

onlyPrompt :: AppTrigger -> AppTrigger
onlyPrompt = controlLens ?~ PromptControlApp

notPrompt :: AppTrigger -> AppTrigger
notPrompt = controlLens ?~ PromptControlItems

triggerPrio :: TriggerPrio -> AppTrigger -> AppTrigger
triggerPrio new = \case
  AppMapping _ lhs dom -> AppMapping new lhs dom
  AppPrompt _ ctrl -> AppPrompt new ctrl

builtinPrio :: AppTrigger -> AppTrigger
builtinPrio = triggerPrio 0

userPrio :: AppTrigger -> AppTrigger
userPrio = triggerPrio userPrioValue

type MenuApp s r result =
  Map AppTrigger (MenuWidget s r result)

inputMatrix :: AppTrigger -> MenuWidget s r result -> [(InputParams, (TriggerPrio, MenuWidget s r result))]
inputMatrix (AppMapping prio lhs InputDomain {..}) widget =
  [
    (InputParams {trigger = InputMapping lhs, mode = InputMode {mode = m, prompt = p}}, (prio, widget))
    | m <- toList modes
    , p <- controls prompt
  ]
  where
    controls = \case
      Just p -> [p]
      Nothing -> [PromptControlApp, PromptControlItems]
inputMatrix (AppPrompt prio control) widget =
  [
    (InputParams {trigger = InputPrompt, mode = InputMode {mode = m, prompt = control}}, (prio, widget))
    | m <- [MapNormal, MapInsert]
  ]

appDispatch :: MenuApp s r result -> InputDispatch s r result
appDispatch app =
  flip Map.lookup matrix
  where
    matrix = snd <$> Map.fromListWith choose (uncurry inputMatrix =<< Map.toList app)

    choose l@(prioL, _) r@(prioR, _) | prioL > prioR = l
                                     | otherwise = r

appMappings :: MenuApp s r result -> [MappingSpec]
appMappings app =
  flip mapMaybe (Map.keys app) \case
    AppMapping _ lhs InputDomain {modes} -> Just (MappingSpec lhs modes)
    AppPrompt _ _ -> Nothing

builtinHandlers :: MenuApp s r result
builtinHandlers =
  Map.mapKeys builtinPrio [
    (withInsert "<esc>", menuEsc),
    (withInsert "<c-c>", menuQuit),
    (AppPrompt 0 PromptControlItems, menuOk)
  ]

defaultHandlers ::
  MenuState s =>
  MenuApp s r result
defaultHandlers =
  Map.mapKeys builtinPrio [
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
  Map.insert (AppPrompt userPrioValue PromptControlApp) widget

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
