module Ribosome.Menu.Prompt.Nvim where

import qualified Data.Text as Text (singleton, splitAt, uncons)
import qualified Polysemy.Conc as Conc
import qualified Polysemy.Time as Time
import Prelude hiding (consume)
import qualified Streamly.Prelude as Stream

import Ribosome.Api.Window (redraw)
import Ribosome.Final (inFinal_)
import qualified Ribosome.Host.Api.Data as ApiData (vimCommand)
import Ribosome.Host.Api.Effect (nvimInput, vimCallFunction, vimCommand, vimCommandOutput, vimGetOption, vimSetOption)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (silentBang)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent (InputEvent (..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptInput (PromptInput))
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer))
import Ribosome.Text (escapeQuotes)

quitChar :: Char
quitChar =
  '†'

quitCharOrd :: Int
quitCharOrd =
  ord quitChar

getChar ::
  Members [Rpc !! RpcError, Rpc, Race, Embed IO] r =>
  MVar () ->
  Sem r InputEvent
getChar quit =
  resumeAs @RpcError InputEvent.Interrupt consume
  where
    consume =
      either pure event =<< Conc.race waitQuit (getchar [])
    waitQuit =
      embed (readMVar quit) *> getchar [toMsgpack True] $> InputEvent.Interrupt
    getchar =
      vimCallFunction "getchar"
    event (Right c) =
      pure (InputEvent.Character (fromMaybe c (decodeInputChar c)))
    event (Left 0) =
      pure InputEvent.NoInput
    event (Left num) | num == quitCharOrd =
      pure InputEvent.Interrupt
    event (Left num) =
      maybe (InputEvent.Unexpected num) InputEvent.Character <$> decodeInputNum num

getCharStream ::
  TimeUnit u =>
  Members [Rpc, Rpc !! RpcError, Time t d, Race, Embed IO, Final IO] r =>
  u ->
  Sem r PromptInput
getCharStream interval =
  inFinal_ \ lowerMaybe lower_ pur ->
    pur $ PromptInput \ quit -> do
      let
        run =
          maybe run check =<< Stream.fromEffect (lowerMaybe (getChar quit))
        check = \case
          InputEvent.Character a ->
            Stream.cons (PromptInputEvent.Character a) run
          InputEvent.Interrupt ->
            Stream.fromPure PromptInputEvent.Interrupt
          InputEvent.Error e ->
            Stream.fromPure (PromptInputEvent.Error e)
          InputEvent.NoInput ->
            Stream.before (lower_ (Time.sleep interval)) run
          InputEvent.Unexpected _ ->
            run
      run

promptFragment :: Text -> Text -> [Text]
promptFragment hl text =
  ["echohl " <> hl, "echon '" <> escapeQuotes text <> "'"]

nvimRenderPrompt ::
  Member Rpc r =>
  Prompt ->
  Sem r ()
nvimRenderPrompt (Prompt cursor _ (PromptText text)) =
  silentBang do
    Rpc.sync (fold calls)
  where
    calls = ApiData.vimCommand <$> ("redraw!" : (fragments >>= uncurry promptFragment))
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

newtype NvimPromptResources =
  NvimPromptResources {
    _guicursor :: Text
  }
  deriving stock (Eq, Show)

nvimAcquire ::
  Members [Rpc, Rpc !! RpcError] r =>
  Sem r NvimPromptResources
nvimAcquire = do
  highlightSet <- resumeAs False (True <$ vimCommandOutput "highlight RibosomePromptCaret")
  unless highlightSet $ vimCommand "highlight link RibosomePromptCaret TermCursor"
  res <- NvimPromptResources <$> vimGetOption "guicursor"
  vimSetOption "guicursor" (toMsgpack ("a:hor20" :: Text))
  () <- vimCallFunction "inputsave" []
  pure res

nvimRelease ::
  Member Rpc r =>
  NvimPromptResources ->
  Sem r ()
nvimRelease (NvimPromptResources gc) = do
  nvimInput "<esc>"
  vimSetOption "guicursor" (toMsgpack gc)
  redraw
  vimCommand "echon ''"
  vimCallFunction "inputrestore" []

nvimPromptRenderer ::
  Members [Rpc, Rpc !! RpcError] r =>
  PromptRenderer r
nvimPromptRenderer =
  PromptRenderer nvimAcquire nvimRelease nvimRenderPrompt
