-----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- All tests.
-----------------------------------------------------------------------------
module Main where

import qualified Database.Kdb.Internal.ClientTest              as ClientTest
import qualified Database.Kdb.Internal.IntegrationTest         as IntegrationTest
import qualified Database.Kdb.Internal.IPCTest                 as IPCTest
import qualified Database.Kdb.Internal.Types.DateTimeTypesTest as DateTimeTypesTest
import qualified Test.Tasty                                    as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [ DateTimeTypesTest.tests
  , IPCTest.tests
  , IntegrationTest.tests
  , ClientTest.tests
  ]
