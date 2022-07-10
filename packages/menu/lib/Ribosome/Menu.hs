module Ribosome.Menu (
  NvimMenu,
  module Ribosome.Menu.Action,
  module Ribosome.Menu.Data.MenuAction,
  module Ribosome.Menu.Data.MenuEvent,
  module Ribosome.Menu.Data.MenuItem,
  module Ribosome.Menu.Effect.MenuFilter,
  module Ribosome.Menu.Data.MenuResult,
  module Ribosome.Menu.Data.MenuState,
  module Ribosome.Menu.Effect.MenuConsumer,
  module Ribosome.Menu.Effect.MenuState,
  module Ribosome.Menu.Effect.MenuStream,
  module Ribosome.Menu.Effect.MenuRenderer,
  module Ribosome.Menu.Effect.NvimPromptInput,
  module Ribosome.Menu.Effect.PromptControl,
  module Ribosome.Menu.Effect.PromptEvents,
  module Ribosome.Menu.Effect.PromptInput,
  module Ribosome.Menu.Effect.PromptRenderer,
  module Ribosome.Menu.Effect.PromptStream,
  module Ribosome.Menu.Interpreter.Menu,
  module Ribosome.Menu.Interpreter.MenuConsumer,
  module Ribosome.Menu.Interpreter.MenuFilter,
  module Ribosome.Menu.Interpreter.MenuState,
  module Ribosome.Menu.Interpreter.MenuStream,
  module Ribosome.Menu.Interpreter.NvimPromptInput,
  module Ribosome.Menu.Interpreter.PromptControl,
  module Ribosome.Menu.Interpreter.PromptInput,
  module Ribosome.Menu.Interpreter.PromptRenderer,
  module Ribosome.Menu.Interpreter.PromptStream,
  module Ribosome.Menu.ItemLens,
  module Ribosome.Menu.Items,
  module Ribosome.Menu.Main,
  module Ribosome.Menu.Nvim,
  module Ribosome.Menu.Prompt.Data.Prompt,
  module Ribosome.Menu.Prompt.Data.PromptFlag,
  module Ribosome.Menu.Prompt.Data.PromptListening,
  module Ribosome.Menu.Prompt.Data.PromptMode,
  module Ribosome.Menu.Prompt.Data.PromptQuit,
  module Ribosome.Menu.Prompt.Run,
) where

import Ribosome.Menu.Action (
  menuCycle,
  menuIgnore,
  menuModify,
  menuNavigate,
  menuOk,
  menuQuit,
  menuRender,
  menuSuccess,
  menuToggle,
  menuToggleAll,
  menuUpdatePrompt,
  )
import Ribosome.Menu.Data.MenuAction
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem), simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult (..))
import Ribosome.Menu.Data.MenuState (MenuWidget, SemS (SemS), semState)
import Ribosome.Menu.Effect.MenuConsumer (MenuConsumer (..))
import Ribosome.Menu.Effect.MenuFilter (Fuzzy, MenuFilter (..), Substring)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer (..), NvimRenderer, withMenuRenderer)
import Ribosome.Menu.Effect.MenuState (
  MenuState,
  putCursor,
  putItems,
  readCursor,
  readItems,
  readPrompt,
  useCursor,
  useItems,
  viewCursor,
  viewItems,
  viewMenu,
  )
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.NvimPromptInput (NvimPromptInput, getChar)
import Ribosome.Menu.Effect.PromptControl (PromptControl, waitPromptListening, waitPromptQuit)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents (..))
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (NvimPrompt, PromptRenderer (..), withPrompt)
import Ribosome.Menu.Effect.PromptStream (PromptStream)
import Ribosome.Menu.Interpreter.Menu
import Ribosome.Menu.Interpreter.MenuConsumer (Mappings, basic, defaultMappings, forMappings, withMappings)
import Ribosome.Menu.Interpreter.MenuFilter (
  BoolVal,
  interpretMenuFilterFuzzy,
  interpretMenuFilterSubstring,
  )
import Ribosome.Menu.Interpreter.MenuState (interpretMenuState, interpretMenuStateDeps, interpretMenuStates)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.NvimPromptInput (interpretNvimPromptInput)
import Ribosome.Menu.Interpreter.PromptControl (interpretPromptControl)
import Ribosome.Menu.Interpreter.PromptInput (
  interpretPromptInputCharList,
  interpretPromptInputList,
  interpretPromptInputQueue,
  nvimPromptInput,
  )
import Ribosome.Menu.Interpreter.PromptRenderer (interpretPromptRendererNull, interpretPromptRendererNvim)
import Ribosome.Menu.Interpreter.PromptStream (interpretPromptStream)
import Ribosome.Menu.ItemLens (
  entriesByIndex,
  focus,
  getFocus,
  itemsByEntryIndex,
  menuItemsByIndexes,
  selected,
  selected',
  selectedItems,
  selectedItemsOnly,
  selectedOnly,
  unselected,
  unselectedItems,
  )
import Ribosome.Menu.Items (
  deleteSelected,
  traverseSelection_,
  withFocus,
  withFocus',
  withFocusItem,
  withSelection,
  withSelection',
  withSelectionItems,
  )
import Ribosome.Menu.Main (menu, simpleMenu)
import Ribosome.Menu.Nvim (ensureSize, interpretNvimMenu, menuScratch, menuScratchSized)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptFlag
import Ribosome.Menu.Prompt.Data.PromptListening
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (..))
import Ribosome.Menu.Prompt.Data.PromptQuit
import Ribosome.Menu.Prompt.Run (pristinePrompt, promptEventStream, withPromptInput)

type NvimMenu i =
  [
    PromptStream,
    MenuFilter,
    MenuStream
  ] ++ NvimMenuDeps i
