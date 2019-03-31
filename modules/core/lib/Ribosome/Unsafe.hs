module Ribosome.Unsafe where

import GHC.IO.Unsafe (unsafePerformIO)

unsafeLogS :: Show a => a -> b -> b
unsafeLogS a b = unsafePerformIO $ print a >> return b

unsafeLog :: String -> b -> b
unsafeLog a b = unsafePerformIO $ putStrLn a >> return b
