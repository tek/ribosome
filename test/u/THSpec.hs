{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoOverloadedStrings #-}

module THSpec(
  htf_thisModulesTests,
) where

-- import Language.Haskell.TH (pprint, runQ)
import Test.Framework

-- import Prelude

-- import Ribosome.Nvim.Api.GenerateData
-- import Ribosome.Nvim.Api.GenerateIO

-- test_io :: IO ()
-- test_io = do
--   expr <- runQ generateIO
--   -- expr <- runQ $ genIO fun
--   -- putStrLn $ show expr
--   putStrLn $ pprint expr
--   return ()

-- test_data :: IO ()
-- test_data = do
--   expr <- runQ generateData
--   -- putStrLn $ show expr
--   putStrLn $ pprint expr
--   return ()
