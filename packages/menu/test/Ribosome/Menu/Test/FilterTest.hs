module Ribosome.Menu.Test.FilterTest where

import Polysemy.Test (UnitTest, runTestAuto, (===))

import Ribosome.Menu.Combinators (sortEntriesText)
import Ribosome.Menu.Data.Filter (Filter (Fuzzy))
import Ribosome.Menu.Data.MenuItem (Items, simpleItems)
import Ribosome.Menu.Effect.MenuFilter (FilterJob (Initial), menuFilter)
import Ribosome.Menu.Interpreter.MenuFilter (interpretFilter)

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
    r <- interpretFilter (menuFilter Fuzzy "ab" (Initial items))
    ["ab", "xabc", "xaxbx", "xaxbxcx"] === sortEntriesText r
