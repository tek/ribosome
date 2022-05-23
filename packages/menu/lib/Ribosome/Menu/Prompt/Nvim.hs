module Ribosome.Menu.Prompt.Nvim where

import qualified Data.Text as Text (singleton, splitAt, uncons)
import qualified Polysemy.Conc as Conc
import Polysemy.Final (withWeavingToFinal)
import qualified Polysemy.Time as Time
import Prelude hiding (consume)
import qualified Streamly.Prelude as Stream

import Ribosome.Api.Window (redraw)
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
      embed (readMVar quit) *> getchar [True] $> InputEvent.Interrupt
    getchar =
      vimCallFunction "getchar" . fmap toMsgpack
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
  Sem r (PromptInput IO)
getCharStream interval =
  withWeavingToFinal \ s wv ex ->
    pure $ (<$ s) $ PromptInput \ quit -> do
      let
        run =
          maybe run check . ex =<< Stream.fromEffect (wv (getChar quit <$ s))
        check = \case
          InputEvent.Character a ->
            Stream.cons (PromptInputEvent.Character a) run
          InputEvent.Interrupt ->
            Stream.fromPure PromptInputEvent.Interrupt
          InputEvent.Error e ->
            Stream.fromPure (PromptInputEvent.Error e)
          InputEvent.NoInput ->
            Stream.before (wv (Time.sleep interval <$ s)) run
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
  Members [Rpc, Rpc !! RpcError, Final IO] r =>
  Sem r (PromptRenderer IO)
nvimPromptRenderer =
  withWeavingToFinal \ s wv _ -> do
    let
      acquire =
        wv (nvimAcquire <$ s)
      release a =
        void (wv (nvimRelease <$> a))
      render p =
        void (wv (nvimRenderPrompt p <$ s))
    pure (PromptRenderer acquire release render <$ s)
