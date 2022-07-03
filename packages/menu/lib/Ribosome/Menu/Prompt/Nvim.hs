module Ribosome.Menu.Prompt.Nvim where

import qualified Data.Text as Text (singleton, splitAt, uncons)
import Prelude hiding (consume)

import Ribosome.Api.Window (redraw)
import qualified Ribosome.Host.Api.Data as ApiData (vimCommand)
import Ribosome.Host.Api.Effect (nvimInput, vimCallFunction, vimCommand, vimCommandOutput, vimGetOption, vimSetOption)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (silentBang)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Text (escapeQuotes)

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
