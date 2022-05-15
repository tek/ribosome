module Ribosome.Menu.Prompt.Nvim where

import Control.Concurrent.Async.Lifted (race)
import qualified Data.Text as Text (singleton, splitAt, uncons)
import qualified Streamly.Prelude as Stream

import Ribosome.Api.Atomic (atomic)
import Ribosome.Api.Window (redraw)
import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent (InputEvent (..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptInput (PromptInput))
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer))
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Host.Api.Data as ApiData (vimCommand)
import Ribosome.Host.Api.Effect (nvimInput, vimCallFunction, vimCommand, vimCommandOutput, vimGetOption, vimSetOption)
import Ribosome.System.Time (sleep)

quitChar :: Char
quitChar =
  '†'

quitCharOrd :: Int
quitCharOrd =
  ord quitChar

getChar ::
  MonadIO m =>
  Member Rpc r =>
  MonadBaseControl IO m =>
  MVar () ->
  m InputEvent
getChar quit =
  catchAs @RpcError InputEvent.Interrupt consume
  where
    consume =
      either pure event =<< race waitQuit (getchar [])
    waitQuit =
      readMVar quit *> getchar [True] $> InputEvent.Interrupt
    getchar =
      vimCallFunction "getchar" . fmap toMsgpack
    event (Right c) =
      pure $ InputEvent.Character (fromMaybe c (decodeInputChar c))
    event (Left 0) =
      pure InputEvent.NoInput
    event (Left num) | num == quitCharOrd =
      pure InputEvent.Interrupt
    event (Left num) =
      maybe (InputEvent.Unexpected num) InputEvent.Character <$> decodeInputNum num

getCharStream ::
  Member Rpc r =>
  MonadBaseControl IO m =>
  Double ->
  PromptInput m
getCharStream interval =
  PromptInput \ quit -> do
    let
      run =
        check =<< Stream.fromEffect (getChar quit)
      check = \case
        InputEvent.Character a ->
          Stream.cons (PromptInputEvent.Character a) run
        InputEvent.Interrupt ->
          Stream.fromPure PromptInputEvent.Interrupt
        InputEvent.Error e ->
          Stream.fromPure (PromptInputEvent.Error e)
        InputEvent.NoInput ->
          Stream.before (sleep interval) run
        InputEvent.Unexpected _ ->
          run
    run

promptFragment :: Text -> Text -> [Text]
promptFragment hl text =
  ["echohl " <> hl, "echon '" <> escapeQuotes text <> "'"]

nvimRenderPrompt ::
  Monad m =>
  Member Rpc r =>
  Prompt ->
  m ()
nvimRenderPrompt (Prompt cursor _ (PromptText text)) =
  void $ atomic calls
  where
    calls = syncRpcCall . ApiData.vimCommand <$> ("silent! redraw!" : (fragments >>= uncurry promptFragment))
    fragments =
      [
        ("RibosomePromptSign", sign),
        ("None", pre),
        ("RibosomePromptCaret", Text.singleton cursorChar),
        ("None", post)
        ]
    (pre, rest) =
      Text.splitAt cursor text
    (cursorChar, post) =
      fromMaybe (' ', "") (Text.uncons rest)
    sign =
      "% "

loopFunctionName :: Text
loopFunctionName =
  "RibosomeMenuLoop"

loopVarName :: Text
loopVarName =
  "ribosome_menu_looping"

newtype NvimPromptResources =
  NvimPromptResources {
    _guicursor :: Text
  }
  deriving stock (Eq, Show)

nvimAcquire ::
  Member Rpc r =>
  m NvimPromptResources
nvimAcquire = do
  highlightSet <- catchAs @RpcError False $ True <$ vimCommandOutput "highlight RibosomePromptCaret"
  unless highlightSet $ vimCommand "highlight link RibosomePromptCaret TermCursor"
  res <- NvimPromptResources <$> vimGetOption "guicursor"
  vimSetOption "guicursor" (toMsgpack ("a:hor20" :: Text))
  () <- vimCallFunction "inputsave" []
  pure res

nvimRelease ::
  MonadIO m =>
  Member Rpc r =>
  NvimPromptResources ->
  m ()
nvimRelease (NvimPromptResources gc) = do
  nvimInput "<esc>"
  vimSetOption "guicursor" (toMsgpack gc)
  redraw
  vimCommand "echon ''"
  vimCallFunction "inputrestore" []

nvimPromptRenderer ::
  MonadIO m =>
  Member Rpc r =>
  PromptRenderer m
nvimPromptRenderer =
  PromptRenderer nvimAcquire nvimRelease nvimRenderPrompt
