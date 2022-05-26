module Ribosome.Menu.Test.StreamTest where

import Hedgehog ((===))
import Polysemy.Test (UnitTest)
import qualified Streamly.Prelude as Stream

import Ribosome.Menu.Stream.Accumulate (mapMAcc)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent (threadDelay)

data ADat =
  Normal Int
  |
  Prio Int
  deriving stock (Eq, Show)

inputAcc :: [ADat]
inputAcc =
  [
    Normal 1,
    Normal 2,
    Prio 3,
    Normal 4,
    Normal 5,
    Normal 6,
    Prio 7,
    Normal 8,
    Normal 9,
    Normal 10
  ]

classifyAcc ::
  Applicative m =>
  ADat ->
  m (Either ADat [ADat])
classifyAcc = pure . \case
  Prio a -> Right [Prio a]
  Normal a -> Left (Normal a)

consumeAcc ::
  NonEmpty ADat ->
  IO [ADat]
consumeAcc ds = do
  threadDelay 50000
  pure (toList ds)

test_mapMAcc :: UnitTest
test_mapMAcc = do
  r <- lift $
    Stream.toList $
    mapMAcc classifyAcc consumeAcc $
    Stream.delay 0.02 $
    Stream.fromList inputAcc
  [
    [Prio 3],
    [Normal 1],
    [Normal 2, Normal 4],
    [Prio 7],
    [Normal 5, Normal 6, Normal 8],
    [Normal 9, Normal 10]
    ] === r
