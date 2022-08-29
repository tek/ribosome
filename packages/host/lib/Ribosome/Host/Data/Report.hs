-- |Data structures related to logging and notifying the user
module Ribosome.Host.Data.Report where

import qualified Data.Text as Text
import Exon (exon)
import Fcf (Pure1, type (@@))
import Fcf.Class.Functor (FMap)
import Polysemy.Log (Severity (Error))
import Prelude hiding (tag)
import Text.Show (showParen, showsPrec)

-- |The provenance of a report, for use in logs.
newtype ReportContext =
  ReportContext { unReportContext :: [Text] }
  deriving stock (Eq, Show)
  deriving newtype (Ord, Semigroup, Monoid)

-- |Render a 'ReportContext' by interspersing it with dots, returning 'Nothing' if it is empty.
reportContext' :: ReportContext -> Maybe Text
reportContext' = \case
  ReportContext [] -> Nothing
  ReportContext c -> Just (Text.intercalate "." c)

-- |Render a 'ReportContext' by interspersing it with dots, followed by a colon, returning 'Nothing' if it is empty.
prefixReportContext' :: ReportContext -> Maybe Text
prefixReportContext' c =
  flip Text.snoc ':' <$> reportContext' c

-- |Render a 'ReportContext' by interspersing it with dots, using @global@ if it is empty.
reportContext :: ReportContext -> Text
reportContext c =
  fromMaybe "global" (reportContext' c)

-- |Render a 'ReportContext' by interspersing it with dots, followed by a colon, using @global@ if it is empty.
prefixReportContext :: ReportContext -> Text
prefixReportContext c =
  Text.snoc (reportContext c) ':'

instance IsString ReportContext where
  fromString =
    ReportContext . pure . toText

-- |An report with different messages intended to be sent to Neovim and the log, respectively.
--
-- Used by request handlers and expected by the RPC dispatcher.
--
-- Also contains the 'Severity' of the report, or minimum log level, which determines whether the report should be
-- logged and echoed in Neovim, and what kind of highlighting should be used in Neovim (red for errors, orange for
-- warnings, none for infomrational errors).
--
-- The log message may span multiple lines.
data Report where
  Report :: HasCallStack => {
    user :: Text,
    log :: [Text],
    severity :: Severity
  } -> Report

instance Show Report where
  showsPrec d Report {..} =
    showParen (d > 10)
    [exon|LogReport { user = #{showsPrec 11 user}, log = #{showsPrec 11 log}, severity = #{showsPrec 11 severity} }|]

instance IsString Report where
  fromString (toText -> s) =
    Report s [s] Error

-- |The type used by request handlers and expected by the RPC dispatcher.
data LogReport =
  LogReport {
    -- |The report
    report :: Report,
    -- |Indicates whether this report may be echoed in Neovim
    echo :: Bool,
    -- |Indicates whether to store this report in the state of 'Ribosome.Reports'
    store :: Bool,
    -- |A list of prefixes used for log messages
    context :: ReportContext
  }
  deriving stock (Show, Generic)

-- |Construct a 'LogReport' error from a single 'Text'.
simple ::
  HasCallStack =>
  Text ->
  LogReport
simple msg =
  withFrozenCallStack do
    LogReport (Report msg [msg] Error) True True mempty

-- |Stop with a 'LogReport'.
basicReport ::
  Member (Stop Report) r =>
  HasCallStack =>
  Text ->
  [Text] ->
  Sem r a
basicReport user log =
  withFrozenCallStack do
    stop (Report user log Error)

instance IsString LogReport where
  fromString :: HasCallStack => String -> LogReport
  fromString (toText -> msg) =
    withFrozenCallStack do
      LogReport (Report msg [msg] Error) True True mempty

-- |The class of types that are convertible to a 'Report'.
--
-- This is used to create a uniform format for handlers, since control flow is passed on to the internal machinery when
-- they return.
-- If an error would be thrown that is not caught by the request dispatcher, the entire plugin would stop, so all 'Stop'
-- and 'Resumable' effects need to be converted to 'Report' before returning (see [Errors]("Ribosome#errors")).
--
-- The combinators associated with this class make this task a little less arduous:
--
-- > data NumbersError = InvalidNumber
-- >
-- > instance Reportable NumbersError where
-- >   toReport InvalidNumber = Report "Invalid number!" ["The user entered an invalid number"] Warn
-- >
-- > count :: Int -> Sem r Int
-- > count i =
-- >   resumeReport @Rpc $ mapReport @NumbersError do
-- >     when (i == 0) (stop InvalidNumber)
-- >     nvimGetVar ("number_" <> show i)
--
-- Here 'resumeReport' converts a potential 'RpcError' from 'Ribosome.Api.nvimGetVar' to 'Report' (e.g. if the variable
-- is not set), while 'mapReport' uses the instance @'Reportable' 'NumbersError'@ to convert the call to 'stop'.
class Reportable e where
  toReport :: e -> Report

instance Reportable Report where
  toReport =
    id

instance Reportable Void where
  toReport = \case

-- |Reinterpret @'Stop' err@ to @'Stop' 'Report'@ if @err@ is an instance of 'Reportable'.
mapReport ::
  ∀ e r a .
  Reportable e =>
  Member (Stop Report) r =>
  Sem (Stop e : r) a ->
  Sem r a
mapReport =
  mapStop toReport

type Stops errs =
  FMap (Pure1 Stop) Fcf.@@ errs

-- |Map multiple errors to 'Report'.
class MapReports (errs :: [Type]) (r :: EffectRow) where
  -- |Map multiple errors to 'Report'.
  -- This needs the errors specified as type applications.
  --
  -- > mapReports @[RpcError, SettingError]
  mapReports :: InterpretersFor (Stops errs) r

instance MapReports '[] r where
  mapReports =
    id

instance (
    Reportable err,
    MapReports errs r,
    Member (Stop Report) (Stops errs ++ r)
  ) => MapReports (err : errs) r where
    mapReports =
      mapReports @errs . mapReport @err

-- |Convert the effect @eff@ to @'Resumable' err eff@ and @'Stop' 'Report'@ if @err@ is an instance of 'Reportable'.
resumeReport ::
  ∀ eff e r a .
  Reportable e =>
  Members [eff !! e, Stop Report] r =>
  Sem (eff : r) a ->
  Sem r a
resumeReport =
  resumeHoist toReport

-- |Resume multiple effects as 'Report's.
class ResumeReports (effs :: EffectRow) (errs :: [Type]) (r :: EffectRow) where
  -- |Resume multiple effects as 'Report's.
  -- This needs both effects and errors specified as type applications (though only the shape for the errors).
  --
  -- > resumeReports @[Rpc, Settings] @[_, _]
  resumeReports :: InterpretersFor effs r

instance ResumeReports '[] '[] r where
  resumeReports =
    id

instance (
    Reportable err,
    ResumeReports effs errs r,
    Members [eff !! err, Stop Report] (effs ++ r)
  ) => ResumeReports (eff : effs) (err : errs) r where
    resumeReports =
      resumeReports @effs @errs . resumeReport @eff @err

-- |Extract both user and log messages from an 'Report', for use in tests.
reportMessages :: Report -> Text
reportMessages Report {user, log} =
  unlines (user : log)

-- |Extract the user message from an instance of 'Reportable'.
userReport ::
  ∀ e .
  Reportable e =>
  e ->
  Text
userReport (toReport -> Report {user}) =
  user

-- |Resume an effect with an error that's an instance of 'Reportable' by passing its user message to a function.
resumeHoistUserMessage ::
  ∀ err eff err' r .
  Reportable err =>
  Members [eff !! err, Stop err'] r =>
  (Text -> err') ->
  InterpreterFor eff r
resumeHoistUserMessage f =
  resumeHoist (f . userReport)

-- |Map an error that's an instance of 'Reportable' by passing its user message to a function.
mapUserMessage ::
  ∀ err err' r .
  Reportable err =>
  Member (Stop err') r =>
  (Text -> err') ->
  InterpreterFor (Stop err) r
mapUserMessage f =
  mapStop (f . userReport)

-- |Convert an error that's an instance of 'Reportable' to 'Fail', for use in tests.
stopReportToFail ::
  ∀ e r .
  Member Fail r =>
  Reportable e =>
  InterpreterFor (Stop e) r
stopReportToFail =
  either (fail . toString . userReport) pure <=< runStop
{-# inline stopReportToFail #-}

-- |Resume an effect with an error that's an instance of 'Reportable' by reinterpreting to 'Fail', for use in tests.
resumeReportFail ::
  ∀ eff err r .
  Members [Fail, eff !! err] r =>
  Reportable err =>
  InterpreterFor eff r
resumeReportFail =
  resuming (fail . toString . userReport)
{-# inline resumeReportFail #-}
