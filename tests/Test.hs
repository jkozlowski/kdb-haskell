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

import qualified Database.Kdb.Internal.TypesTest as TypesTest
import qualified Database.Kdb.Internal.IPCTest   as IPCTest
import qualified Test.Tasty                      as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [
    TypesTest.tests
  , IPCTest.tests
  ]
