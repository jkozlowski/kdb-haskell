-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.TypesTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Database.Kdb.Internal.Types'.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.TypesTest (tests) where

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Database.Kdb.Internal.Types as Types
import           Database.Kdb.Internal.C

tests :: TestTree
tests = testGroup "Database.Kdb.Internal.Types" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "dummy" dummyProperty
  ]

dummyProperty :: Bool
dummyProperty = True
