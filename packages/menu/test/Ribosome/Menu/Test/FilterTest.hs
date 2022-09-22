module Ribosome.Menu.Test.FilterTest where

import Lens.Micro.Mtl (view)
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Ribosome.Menu.Combinators (sortEntriesText)
import Ribosome.Menu.Data.Filter (Filter (Fuzzy))
import Ribosome.Menu.Data.FilterMode (FilterMode (FilterMode))
import Ribosome.Menu.Data.MenuItem (Items, simpleItems)
import Ribosome.Menu.Effect.MenuFilter (FilterJob (Initial), menuFilter)
import Ribosome.Menu.Interpreter.MenuFilter (defaultFilter)

items :: Items ()
items =
  simpleItems [
    "xaxbx",
    "xabc",
    "xaxbxcx",
    "ab"
  ]

test_filterFuzzy :: UnitTest
test_filterFuzzy =
  runTestAuto do
    r <- defaultFilter (menuFilter (FilterMode Fuzzy (Just . view #text)) "ab" (Initial items))
    ["ab", "xabc", "xaxbx", "xaxbxcx"] === sortEntriesText r
