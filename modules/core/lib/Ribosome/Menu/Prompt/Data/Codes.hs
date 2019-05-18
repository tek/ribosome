module Ribosome.Menu.Prompt.Data.Codes where

import Control.Exception.Lifted (try)
import Data.Map (Map, (!?))
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (singleton)

specialCodes :: Map Text Text
specialCodes =
  Map.fromList [
    ("\x80\xffX", "c-@"),
    ("\x80k", "bs"),
    ("\x80kB", "s-ta"),
    ("\x0", "c-k"),
    ("\x80kD", "del"),
    ("\x9B", "csi"),
    ("\x80\xfdP", "xcsi"),
    ("\x80ku", "up"),
    ("\x80kd", "down"),
    ("\x80kl", "left"),
    ("\x80kr", "right"),
    -- ("\x80\xfd", "s-up"),
    -- ("\x80\xfd", "s-down"),
    ("\x80#4", "s-left"),
    ("\x80%i", "s-right"),
    ("\x80\xfdT", "c-left"),
    ("\x80\xfdU", "c-right"),
    ("\x80k1", "f1"),
    ("\x80k2", "f2"),
    ("\x80k3", "f3"),
    ("\x80k4", "f4"),
    ("\x80k5", "f5"),
    ("\x80k6", "f6"),
    ("\x80k7", "f7"),
    ("\x80k8", "f8"),
    ("\x80k9", "f9"),
    ("\x80k;", "f10"),
    ("\x80F1", "f11"),
    ("\x80F2", "f12"),
    ("\x80\xfd\x06", "s-f1"),
    ("\x80\xfd\x07", "s-f2"),
    ("\x80\xfd\x08", "s-f3"),
    ("\x80\xfd\x09", "s-f4"),
    ("\x80\xfd\x0A", "s-f5"),
    ("\x80\xfd\x0B", "s-f6"),
    ("\x80\xfd\x0C", "s-f7"),
    ("\x80\xfd\x0D", "s-f8"),
    ("\x80\xfd\x0E", "s-f9"),
    ("\x80\xfd\x0F", "s-f10"),
    ("\x80\xfd\x10", "s-f11"),
    ("\x80\xfd\x11", "s-f12"),
    ("\x80%1", "help"),
    ("\x80&8", "undo"),
    ("\x80kI", "insert"),
    ("\x80kh", "home"),
    ("\x80@7", "end"),
    ("\x80kP", "pageup"),
    ("\x80kN", "pagedown"),
    ("\x80K1", "khome"),
    ("\x80K4", "kend"),
    ("\x80K3", "kpageup"),
    ("\x80K5", "kpagedown"),
    ("\x80K6", "kplus"),
    ("\x80K7", "kminus"),
    ("\x80K9", "kmultiply"),
    ("\x80K8", "kdivide"),
    ("\x80KA", "kenter"),
    ("\x80KB", "kpoint"),
    ("\x80KC", "k0"),
    ("\x80KD", "k1"),
    ("\x80KE", "k2"),
    ("\x80KF", "k3"),
    ("\x80KG", "k4"),
    ("\x80KH", "k5"),
    ("\x80KI", "k6"),
    ("\x80KJ", "k7"),
    ("\x80KK", "k8"),
    ("\x80KL", "k9")
    ]

specialNumCodes :: Map Int Text
specialNumCodes =
  Map.fromList [
    (9, "ta"),
    (10, "c-j"),
    (11, "c-k"),
    (12, "fe"),
    (13, "cr"),
    (27, "esc"),
    (32, "space"),
    (60, "lt"),
    (92, "bslash"),
    (124, "bar")
    ]

modifierCodes :: [(Int, Text)]
modifierCodes =
  [
    (2, "shift"),
    (4, "control"),
    (8, "alt"),
    (16, "meta"),
    (32, "mouse_double"),
    (64, "mouse_triple"),
    (96, "mouse_quadruple"),
    (128, "command")
    ]

decodeInputChar :: Text -> Maybe Text
decodeInputChar =
  (specialCodes !?)

decodeInputNum ::
  MonadBaseControl IO m =>
  Int ->
  m (Maybe Text)
decodeInputNum a =
  maybe codepoint (return . Just) (specialNumCodes !? a)
  where
    codepoint =
      fmap Text.singleton . rightToMaybe @SomeException <$> try (return $ chr a)
