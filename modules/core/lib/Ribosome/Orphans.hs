{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ribosome.Orphans where

import Chiasma.Data.Cmd (Cmds(Cmds))
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import Chiasma.Data.TmuxError (TmuxError)
import qualified Chiasma.Data.TmuxError as TmuxError (TmuxError(..))
import Chiasma.Data.Views (ViewsError(..))
import Chiasma.Ui.Data.TreeModError (TreeModError(..))
import Chiasma.Ui.Data.View (View(View))
import Control.Monad.Catch (MonadCatch(..), MonadMask(..))
import Neovim.Context.Internal (Neovim(..))
import System.Log.Logger (Priority(ERROR, DEBUG, NOTICE))

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))

deriving instance MonadCatch (Neovim e)
deriving instance MonadMask (Neovim e)

invalidOutput :: Text
invalidOutput = "invalid output from tmux process"

instance ReportError TmuxError where
  errorReport (TmuxError.ProcessFailed (Cmds cmds) reason) =
    ErrorReport "fatal error in tmux process" log' ERROR
    where
      log' = ["tmux process failed:", reason, "commands:"] <> (show <$> cmds)
  errorReport (TmuxError.OutputParsingFailed (Cmds cmds) output parseError) =
    ErrorReport invalidOutput (["tmux output parsing failed:"] <> (show <$> cmds) <> ["output:"] <>
    output <> ["parse error:", show parseError]) ERROR
  errorReport (TmuxError.NoOutput (Cmds cmds)) =
    ErrorReport invalidOutput ("no output from tmux process:" : (show <$> cmds)) ERROR
  errorReport (TmuxError.DecodingFailed (Cmds cmds) output decodeError) =
    ErrorReport invalidOutput ("failed to decode tmux process output:" : (show <$> cmds) <> ["output:", output,
      "decoding error:", show decodeError]) ERROR
  errorReport (TmuxError.InvalidOutput reason cmd) =
    ErrorReport invalidOutput ["invalid output from tmux process:", toText reason, toText cmd] ERROR
  errorReport (TmuxError.CommandFailed _ err) =
    ErrorReport invalidOutput ("tmux command failed:" : err) ERROR

viewExists :: Text -> View a -> ErrorReport
viewExists desc (View ident _ _ _) =
  ErrorReport msg [msg] DEBUG
  where
    msg = "a " <> desc <> " with ident `" <> identText ident <> "` already exists"

viewMissing :: Text -> Ident -> ErrorReport
viewMissing desc ident =
  ErrorReport msg [msg] DEBUG
  where
    msg = "no " <> desc <> " with ident `" <> identText ident <> "`"

ambiguousView :: Text -> Ident -> Int -> ErrorReport
ambiguousView desc ident num =
  ErrorReport msg [logMsg] ERROR
  where
    msg = "there are " <> show num <> " " <> desc <> "s with ident `" <> identText ident <> "`"
    logMsg = "ambiguous " <> desc <> ": " <> identText ident <> "(" <> show num <> ")"

instance ReportError TreeModError where
  errorReport (PaneExists pane) =
    viewExists "pane" pane
  errorReport (LayoutExists layout) =
    viewExists "layout" layout
  errorReport (PaneMissing pane) =
    viewMissing "pane" pane
  errorReport (LayoutMissing layout) =
    viewMissing "layout" layout
  errorReport (AmbiguousPane pane num) =
    ambiguousView "pane" pane num
  errorReport (AmbiguousLayout layout num) =
    ambiguousView "layout" layout num
  errorReport NoTrees =
    ErrorReport msg [msg] DEBUG
    where
      msg = "no UI layouts have been created"

noSuchView :: Text -> Ident -> ErrorReport
noSuchView desc ident =
  ErrorReport msg [msg] NOTICE
  where
    msg = "no tmux " <> desc <> " with ident `" <> identText ident <> "`"

noId :: Text -> Ident -> ErrorReport
noId desc ident =
  ErrorReport msg [msg] ERROR
  where
    msg = "tmux " <> desc <> " with ident `" <> identText ident <> "`" <> " has no id"

instance ReportError ViewsError where
  errorReport (NoSuchSession ident) =
    noSuchView "session" ident
  errorReport (NoSuchWindow ident) =
    noSuchView "window" ident
  errorReport (NoSuchPane ident) =
    noSuchView "pane" ident
  errorReport (NoPaneId ident) =
    noId "pane" ident

instance ReportError RenderError where
  errorReport (RenderError.NoPrincipal ident) =
    ErrorReport "internal render error: no view in layout" ["no principal in " <> identText ident] ERROR
  errorReport (RenderError.Views err) =
    errorReport err
  errorReport (RenderError.Pack message) =
    ErrorReport ("error packing a tmux layout: " <> toText message) ["tmux pack error:", toText message] ERROR
  errorReport (RenderError.Fatal tmuxError) =
    errorReport tmuxError
