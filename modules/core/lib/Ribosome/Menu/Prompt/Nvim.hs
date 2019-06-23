module Ribosome.Menu.Prompt.Nvim where

import Conduit (ConduitT, yield)
import Control.Exception.Lifted (bracket_)
import Control.Monad.DeepError (ignoreError)
import qualified Data.Text as Text (singleton, splitAt, uncons)

import Ribosome.Api.Atomic (atomic)
import Ribosome.Api.Function (defineFunction)
import Ribosome.Api.Variable (setVar)
import Ribosome.Api.Window (redraw)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent (InputEvent(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer(PromptRenderer))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Nvim.Api.Data as ApiData (vimCommand)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimCommandOutput, vimGetOption, vimSetOption)
import Ribosome.Nvim.Api.RpcCall (RpcError, syncRpcCall)
import Ribosome.System.Time (sleep)

quitChar :: Char
quitChar =
  '†'

quitCharOrd :: Int
quitCharOrd =
  ord quitChar

getChar ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  m InputEvent
getChar =
  catchAs @RpcError InputEvent.Interrupt request
  where
    request =
      ifM peek consume (return InputEvent.NoInput)
    peek =
      (/= (0 :: Int)) <$> getchar True
    consume =
      event =<< getchar False
    getchar peek' =
      vimCallFunction "getchar" [toMsgpack peek']
    event (Right c) =
      return $ InputEvent.Character (fromMaybe c (decodeInputChar c))
    event (Left 0) =
      return InputEvent.NoInput
    event (Left num) | num == quitCharOrd =
      return InputEvent.Interrupt
    event (Left num) =
      maybe (InputEvent.Unexpected num) InputEvent.Character <$> decodeInputNum num

getCharC ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  Double ->
  ConduitT () PromptEvent m ()
getCharC interval =
  recurse
  where
    recurse =
      translate =<< lift getChar
    translate (InputEvent.Character a) =
      yield (PromptEvent.Character a) *> recurse
    translate InputEvent.Interrupt =
      yield PromptEvent.Interrupt
    translate (InputEvent.Error e) =
      yield (PromptEvent.Error e)
    translate InputEvent.NoInput =
      sleep interval *> recurse
    translate (InputEvent.Unexpected _) =
      recurse

promptFragment :: Text -> Text -> [Text]
promptFragment hl text =
  ["echohl " <> hl, "echon '" <> escapeQuotes text <> "'"]

nvimRenderPrompt ::
  Monad m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  Prompt ->
  m ()
nvimRenderPrompt (Prompt cursor _ text) =
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

defineLoopFunction ::
  NvimE e m =>
  m ()
defineLoopFunction =
  defineFunction loopFunctionName [] lns
  where
    lns =
      [
        "echo ''",
        "while g:" <> loopVarName,
        "try",
        "sleep 5m",
        "catch /^Vim:Interrupt$/",
        "silent! call feedkeys('" <> Text.singleton quitChar <> "')",
        "endtry",
        "endwhile"
        ]

startLoop ::
  NvimE e m =>
  m ()
startLoop = do
  defineLoopFunction
  setVar loopVarName True
  vimCommand $ "call feedkeys(\":call " <> loopFunctionName <> "()\\<cr>\")"

-- FIXME need to wait for the loop to stop before deleting the function
killLoop ::
  NvimE e m =>
  m ()
killLoop = do
  setVar loopVarName False
  ignoreError @RpcError $ vimCommand $ "delfunction! " <> loopFunctionName

promptBlocker ::
  NvimE e m =>
  MonadBaseControl IO m =>
  m a ->
  m a
promptBlocker =
  bracket_ startLoop killLoop

newtype NvimPromptResources =
  NvimPromptResources {
    _guicursor :: Text
  }
  deriving (Eq, Show)

nvimAcquire ::
  NvimE e m =>
  m NvimPromptResources
nvimAcquire = do
  highlightSet <- catchAs @RpcError False $ True <$ vimCommandOutput "highlight RibosomePromptCaret"
  unless highlightSet $ vimCommand "highlight link RibosomePromptCaret TermCursor"
  res <- NvimPromptResources <$> vimGetOption "guicursor"
  vimSetOption "guicursor" (toMsgpack ("a:hor20" :: Text))
  () <- vimCallFunction "inputsave" []
  startLoop
  return res

nvimRelease ::
  NvimE e m =>
  MonadRibo m =>
  NvimPromptResources ->
  m ()
nvimRelease (NvimPromptResources gc) = do
  vimSetOption "guicursor" (toMsgpack gc)
  redraw
  vimCommand "echon ''"
  () <- vimCallFunction "inputrestore" []
  killLoop

nvimPromptRenderer ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e DecodeError m =>
  PromptRenderer m
nvimPromptRenderer =
  PromptRenderer nvimAcquire nvimRelease nvimRenderPrompt
