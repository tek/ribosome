module Ribosome.Menu.Data.MenuResult where

import Exon (exon)

data MenuResult a =
  Success a
  |
  Aborted
  |
  Error Text
  deriving stock (Eq, Show, Functor)

describe :: MenuResult a -> Text
describe = \case
  Success _ -> "Success"
  Aborted -> "Aborted"
  Error e -> [exon|Error #{e}|]
