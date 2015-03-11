-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.DateTimeTypesTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Database.Kdb.Internal.DateTimeTypes'.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.Types.DateTimeTypesTest (tests) where

import           Data.Time                       (fromGregorian)
import           Database.Kdb
import           Database.Kdb.Internal.TestUtils (DayPrecision (..),
                                                  TimePrecision (..),
                                                  dayInScale,
                                                  nominalDiffTimeWithPrecision,
                                                  todsInScale, utcTimeInScale,
                                                  (@~=))
import           Test.QuickCheck                 (forAll)
import           Test.QuickCheck.IO              (propertyIO)
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.HUnit                (testCase, (@?=))
import           Test.Tasty.QuickCheck           (testProperty)

tests :: TestTree
tests = testGroup "Database.Kdb.Internal.DateTimeTypesTest" [ unitTests, qcProps ]

unitTests :: TestTree
unitTests = testGroup "HUnit" [
    testCase "kdbEpochDay"     $ show kdbEpochDay     @?= "2000-01-01"
  , testCase "kdbEpochUTCTime" $ show kdbEpochUTCTime @?= "2000-01-01 00:00:00 UTC"
  , testGroup "kdbDate sanity checks" [
        testCase "toDate 2000-01-02" $ fromIntegral (toDate $ fromGregorian 2000 01 02) @?= (1 :: Int)
      , testCase "toDate 1999-12-31" $ fromIntegral (toDate $ fromGregorian 1999 12 31) @?= (-1 :: Int)
      ]
  ]

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [
    testProperty "(x == fromTimestamp . toTimestamp $ x) for nanosecond times" $
              forAll (utcTimeInScale NanoP) $ \utc' -> propertyIO $ (fromTimestamp . toTimestamp $ utc') @?= utc'
  , testProperty "(x == fromMonth . toMonth $ x) for dates accurate to month" $
                forAll (dayInScale MonthP) $ \day' -> propertyIO $ (fromMonth . toMonth $ day') @?= day'
  , testProperty "(x == fromDate . toDate $ x) for dates accurate to day" $
                  forAll (dayInScale DayP) $ \day' -> propertyIO $ (fromDate . toDate $ day') @?= day'
    -- | This test is problematic on day boundaries, because of innacuracy.
  , testProperty "(x == fromDateTime . toDateTime $ x) for second times" $
              forAll (utcTimeInScale SecondP) $ \utc' -> propertyIO $ (fromDateTime . toDateTime $ utc') @~= utc'
  , testProperty "(x == fromTimespan . toTimespan $ x) for nanosecond times" $
              forAll (nominalDiffTimeWithPrecision NanoP) $ \diff' -> propertyIO $ (fromTimespan . toTimespan $ diff') @?= diff'
  , testProperty "(x == fromMinute . toMinute $ x) for minute times" $
                forAll (nominalDiffTimeWithPrecision MinuteP) $ \diff' -> propertyIO $ (fromMinute . toMinute $ diff') @?= diff'
  , testProperty "(x == fromSecond . toSecond $ x) for second times" $
                forAll (nominalDiffTimeWithPrecision SecondP) $ \diff' -> propertyIO $ (fromSecond . toSecond $ diff') @?= diff'
  , testProperty "(x == fromTime . toTime $ x) for millisecond times" $
        forAll (todsInScale MilliP) $ \tod -> propertyIO $ (fromTime . toTime $ tod) @?= tod
  ]
