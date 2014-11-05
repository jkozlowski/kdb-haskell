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

import qualified Database.Kdb.Internal.DateTimeTypesTest as DateTimeTypesTest
import qualified Database.Kdb.Internal.IntegrationTest   as IntegrationTest
import qualified Database.Kdb.Internal.IPCTest           as IPCTest
import qualified Language.Q.LexerTest                    as Lexer
import qualified Test.Tasty                              as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [ DateTimeTypesTest.tests
  , IntegrationTest.tests
  , IPCTest.tests
  , Lexer.tests
  ]
