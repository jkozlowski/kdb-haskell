{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.TypesTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Database.Kdb.Internal.IPC'.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.IPCTest (tests) where

import qualified Blaze.ByteString.Builder as Blaze
import           Control.Applicative      ((<$>), (<*))
import           Control.DeepSeq          (deepseq)
import qualified Data.Attoparsec          as A
import qualified Data.ByteString          as B
import           Data.ByteString.Base16   (decode, encode)
import           Data.ByteString.Char8    (ByteString, unpack)
import           Data.Time                (Day, NominalDiffTime, TimeOfDay (..),
                                           UTCTime (..), diffUTCTime,
                                           fromGregorian, timeOfDayToTime,
                                           timeOfDayToTime)
import           Database.Kdb             (Value, bool, boolV, byte, byteV,
                                           char, charV, date, dateTime,
                                           dateTimeV, dateV, dict, float,
                                           floatV, int, intV, list, long, longV,
                                           longVV, minute, minuteV, month,
                                           monthV, real, realV, s, second,
                                           secondV, short, shortV, symV, symVV,
                                           table, time, timeV, timespan,
                                           timespanV, timestamp, timestampV)
import qualified Database.Kdb             as Kdb
import qualified System.Endian            as End
import           Test.Tasty
import           Test.Tasty.HUnit         (assertEqual, assertFailure, testCase)

tests :: TestTree
tests = testGroup "Database.Kdb.Internal.IPC" [ qcProps, unitTests ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
    testGroup "Serialisation"   $ testCases serializationTest
  , testGroup "Deserialisation" $ testCases deserializationTest
  ]

type SimpleTestCase = (String, Value, ByteString)

testCases :: (SimpleTestCase -> TestTree) -> [TestTree]
testCases f = f <$> t

serializationTest :: SimpleTestCase -> TestTree
serializationTest (msg, actual, expected) = testCase msg $ do
        let actualIPC    = Blaze.toByteString $ Kdb.asyncIPC actual
            msg' = unlines [
                msg
              , "   actual  =" ++ (unpack . encode $ actualIPC)
              , "   expected=" ++ unpack expected
              , "   expectedValue=" ++ show actual
              ]
        assertEqual msg'  (fst . decode $ expected) actualIPC

deserializationTest :: SimpleTestCase -> TestTree
deserializationTest (msg, actual, expected) = testCase msg $ do
        let actualDecoded = A.parseOnly (Kdb.ipcParser <* A.endOfInput) (fst . decode $ expected)
        case actualDecoded of
          Left m  -> assertFailure m
          Right v -> assertEqual msg v actual

-- | Day used for tests.
--
-- Needs to be the first day and month to not upset some tests.
testDay :: Day
testDay = fromGregorian 2014 1 1

-- | UTCTime used for tests.
testUtcTimeToSecond :: UTCTime
testUtcTimeToSecond = UTCTime testDay $ timeOfDayToTime $ TimeOfDay 19 38 38

-- | UTCTime used for tests.
testUtcTimeToNanos :: UTCTime
testUtcTimeToNanos = UTCTime testDay $ timeOfDayToTime $ TimeOfDay 19 38 38.312312323

testNominalDiffTimeToNano :: NominalDiffTime
testNominalDiffTimeToNano =
  let midnight = UTCTime testDay 0
      theTime  = UTCTime testDay $ timeOfDayToTime (TimeOfDay 12 12 12.123456789)
  in diffUTCTime theTime midnight

testNominalDiffTimeToMinute :: NominalDiffTime
testNominalDiffTimeToMinute =
  let midnight = UTCTime testDay 0
      theTime  = UTCTime testDay $ timeOfDayToTime (TimeOfDay 12 12 0)
  in diffUTCTime theTime midnight

testNominalDiffTimeToSecond :: NominalDiffTime
testNominalDiffTimeToSecond =
  let midnight = UTCTime testDay 0
      theTime  = UTCTime testDay $ timeOfDayToTime (TimeOfDay 12 12 12)
  in diffUTCTime theTime midnight

todToMilli :: TimeOfDay
todToMilli = TimeOfDay 12 12 12.123

-- | Test cases for all types.
--
-- System endianess gets prepended.
-- TODO: add tests for empty vectors.
t :: [SimpleTestCase]
t =
  let prepare (a, b, c) = (a, deepseq b b, B.append end c)
      end = case End.getSystemEndianness of
              End.BigEndian    -> "00"
              End.LittleEndian -> "01"
  in prepare <$> [
        -- KBool
      ( "q)-8!\"b\"1"
      , bool True
      , "0000000A000000FF01"
      )
        -- KByte
    , ( "q)-8!\"x\"$97"
      , byte 97
      , "0000000A000000FC61"
      )
        -- KShort
    , ( "q)-8!\"h\"$12"
      , short 12
      , "0000000B000000FB0C00"
      )
        -- KInt
    , ( "q)-8!\"i\"$100000000"
      , int 100000000
      , "0000000D000000FA00E1F505"
      )
        -- KLong
    , ( "q)-8!\"j\"$123456789012345678"
      , long (123456789012345678 :: Int)
      , "00000011000000F94EF330A64B9BB601"
      )
        -- KReal
    , ( "q)-8!\"e\"$3.14"
      , real 3.14
      , "0000000D000000F8C3F54840"
      )
        -- KFloat
    , ( "q)-8!\"f\"$1.6180"
      , float 1.6180
      , "00000011000000F717D9CEF753E3F93F"
      )
        -- KChar
    , ( "q)-8!\"c\"$30"
      , char 'c'
      , "0000000A000000F663"
      )
        -- KSymbol
    , ( "q)-8!`helloword"
      , s "helloworld"
      , "00000014000000F568656C6C6F776F726C6400"
      )
    ,   -- KTimestamp
      ( "q)-8!\"P\"$\"2014-1-1T19:38:38.312312323\""
      , timestamp testUtcTimeToNanos
      , "00000011000000F4036E082830042206"
      )
        -- KMonth
    , ( "q)-8!\"M\"$\"2014/01\""
      , month testDay
      , "0000000D000000F3A8000000"
      )
    , ( "q)-8!\"D\"$\"2014/1/1\""
      , date testDay
      , "0000000D000000F2FA130000"
      )
        -- KDateTime
    , ( "q)-8!\"Z\"$\"2014-1-1T19:38:38"
      , dateTime testUtcTimeToSecond
      , "00000011000000F1AB9FE988D1FAB340"
      )
        -- KTimespan
    , ( "q)-8!\"N\"$\"12:12:12.123456789\""
      , timespan testNominalDiffTimeToNano
      , "00000011000000F015E59CBEF4270000"
      )
        -- KMinute
    , ( "q)-8!\"U\"$\"12:12\""
      , minute testNominalDiffTimeToMinute
      , "0000000D000000EFDC020000"
      )
        -- KSecond
    , ( "q)-8!\"V\"$\"12:12:12\""
      , second testNominalDiffTimeToSecond
      , "0000000D000000EE9CAB0000"
      )
        -- Ktime
    , ( "q)-8!\"T\"$\"12:12:12:123\""
      , time todToMilli
      , "0000000D000000EDDB599E02"
      )
        -- KBoolV
    , ( "q)-8!enlist 0b"
      , boolV [False]
      , "0000000F00000001000100000000"
      )
        -- KByteV
    , ( "q)-8!`byte$til 5"
      , byteV [0..4]
      , "000000130000000400050000000001020304"
      )
        -- KShortV
    , ( "q)-8!`short$til 5"
      , shortV [0..4]
      , "0000001800000005000500000000000100020003000400"
      )
        -- KIntV
    , ( "q)-8!`int$til 5"
      , intV [0..4]
      , "000000220000000600050000000000000001000000020000000300000004000000"
      )
        -- KLongV
    , (  "q)-8!`long$til 5"
      , longV [0..4]
      , "0000003600000007000500000000000000000000000100000000000000020000000000000003000000000000000400000000000000"
      )
        -- KRealV
    , ( "q)-8!`real$til 5"
      , realV [0..4]
      , "00000022000000080005000000000000000000803F000000400000404000008040"
      )
        -- KFloatV
    , ( "q)-8!`float$til 5"
      , floatV [0..4]
      , "000000360000000900050000000000000000000000000000000000F03F000000000000004000000000000008400000000000001040"
      )
        -- KCharV
    , ( "q)-8!\"Hello\""
      , charV "Hello"
      , "000000130000000A000500000048656C6C6F"
      )
        -- KSymV
    , ( "q)-8!`Hello`World"
      , symV ["Hello", "World"]
      , "0000001A0000000B000200000048656C6C6F00576F726C6400"
      )
        -- KTimestampV
    , ( "q)-8!enlist \"P\"$\"2014-1-1T19:38:38.312312323\""
      , timestampV [testUtcTimeToNanos]
      , "000000160000000C0001000000036E082830042206"
      )
        -- KMonthV
    , ( "q)-8!enlist \"M\"$\"2014/01\""
      , monthV [testDay]
      , "000000120000000D0001000000A8000000"
      )
        -- KDateV
    , ( "q)-8!enlist \"D\"$\"2014/1/1\""
      , dateV [testDay]
      , "000000120000000E0001000000FA130000"
      )
        -- KDateTimeV
    , ( "q)-8!enlist \"Z\"$\"2014-1-1T19:38:38"
      , dateTimeV [testUtcTimeToSecond]
      , "000000160000000F0001000000AB9FE988D1FAB340"
      )
        -- KTimespanV
    , ( "q)-8!enlist \"N\"$\"12:12:12.123456789\""
      , timespanV [testNominalDiffTimeToNano]
      , "0000001600000010000100000015E59CBEF4270000"
      )
        -- KMinuteV
    , ( "q)-8!enlist \"U\"$\"12:12\""
      , minuteV [testNominalDiffTimeToMinute]
      , "00000012000000110001000000DC020000"
      )
        -- KSecondV
    , ( "q)-8!enlist \"V\"$\"12:12:12\""
      , secondV [testNominalDiffTimeToSecond]
      , "000000120000001200010000009CAB0000"
      )
        -- KTimeV
    , ( "q)-8!enlist \"T\"$\"12:12:12:123\""
      , timeV [todToMilli]
      , "00000012000000130001000000DB599E02"
      )
        -- KList
    , ( "q)-8!enlist each 2 3"
      , list [longV [2], longV [3]]
      , "0000002A00000000000200000007000100000002000000000000000700010000000300000000000000"
      )
        -- KDict
    , ( "q)-8!`a`b!2 3"
      , dict (symVV ["a", "b"]) (longVV [2, 3])
      , "00000029000000630B00020000006100620007000200000002000000000000000300000000000000"
      )
        -- KTable
    , ( "q)-8!'(flip`a`b!enlist each 2 3;([]a:enlist 2;b:enlist 3))"
      , table [symV ["a", "b"], list [intV [2], intV[3]]]
      , "0000002f0000006200630b0002000000610062000000020000000600010000000200000006000100000003000000"
      )
    ]
