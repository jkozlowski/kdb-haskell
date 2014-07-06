-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.TypesTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Database.Kdb.TypesTest'.
-----------------------------------------------------------------------------
module Database.Kdb.TypesTest (tests) where

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Database.Kdb.Types" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "dummy" dummyProperty
  ]

dummyProperty :: Bool
dummyProperty = True
