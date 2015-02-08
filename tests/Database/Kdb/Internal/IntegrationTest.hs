{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.IntegrationTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Integration tests that startup an external kdb instance,
-- pump many messages into it, retrieve them back and compare
-- those to what was sent.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.IntegrationTest (tests) where

import           Control.Monad                        (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Resource         (runResourceT)
import           Data.Monoid                          ((<>))
import qualified Database.Kdb.Internal.Client         as Client
import           Database.Kdb.Internal.TestUtils      (kdbConnection, pass,
                                                       randomRows, schema,
                                                       startKdb, tableName,
                                                       user)
import           Database.Kdb.Internal.Types.KdbTypes
import qualified Database.Kdb.Internal.Types.KdbTypes as KT
import           Prelude                              hiding (FilePath)
import           Test.Tasty
import           Test.Tasty.HUnit                     (Assertion, testCase,
                                                       (@?=))

tests :: TestTree
tests = testGroup "Database.Kdb.Internal.IntegrationTest" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "HUnit" [
    testCase "Integration Test" integrationTest
  ]

-----------------------------------
-- Tests

-- | Starts up a kdb process and connects to it to
-- send a bunch of messages and select them to compare.
integrationTest :: Assertion
integrationTest = do
  let numRows = 100

  runResourceT $ do

    -- Startup kdb
    freePort <- startKdb

    -- Connect to kdb
    con <- kdbConnection user pass freePort

    -- send the schema
    lift . liftIO $ Client.writeKdb (charV schema) con

    -- Gen the rows
    rows <- lift $ randomRows numRows

    -- Send the rows
    lift $ forM_ rows $ \x -> Client.writeKdb x con

    -- Select the rows
    value <- lift $ Client.query selectCount con
    lift $ value @?= KT.long numRows

-----------------------------------
-- Private utility functions

-- | `count select from testTable` statement.
selectCount :: Value
selectCount = charV $ "count select from " <> tableName
