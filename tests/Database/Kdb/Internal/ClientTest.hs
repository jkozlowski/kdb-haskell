{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.ClientTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for @Client@.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.ClientTest (tests) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Resource
import           Database.Kdb                    (InvalidCredentials (..),
                                                  charV, close, writeKdb)
import           Database.Kdb.Internal.TestUtils (assertException, findFreePort,
                                                  kdbConnection, pass, schema,
                                                  startKdb, user)
import           Test.Tasty
import           Test.Tasty.HUnit                (Assertion, testCase)


tests :: TestTree
tests = testGroup "Database.Kdb.Internal.ClientTest" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "HUnit" [
    testCase "Invalid credentials" invalidCredentialsTest
  , testCase "Unknown host" unknownHostTest
  , testCase "Use after close" useAfterCloseTest
  ]

-- | Starts up a kdb process and tries to connect to it with invalid credentials.
invalidCredentialsTest :: Assertion
invalidCredentialsTest = runResourceT $ do
  -- Startup kdb
  freePort <- startKdb

  -- Connect to kdb
  assertException (\(_::InvalidCredentials) -> True) (kdbConnection "someuser" "somepass" freePort)

-- | Starts up a kdb process and tries to connect to it with invalid credentials.
unknownHostTest :: Assertion
unknownHostTest = runResourceT $ do
  -- Startup kdb
  freePort <- lift . liftIO $ findFreePort

  -- Connect to kdb and expect @IOError@.
  assertException (\(_::IOError) -> True) (kdbConnection "someuser" "somepass" freePort)

useAfterCloseTest :: Assertion
useAfterCloseTest = runResourceT $ do

    -- Startup kdb
    freePort <- startKdb

    -- Connect to kdb
    con <- kdbConnection user pass freePort

    -- Close the connection immediately
    close con

    -- Try to send something
    assertException (\(_::IOError) -> True) (writeKdb (charV schema) con)

