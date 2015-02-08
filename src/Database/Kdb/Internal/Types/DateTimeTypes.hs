{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.DateTimeTypes
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Types and utility functions to convert to/from kdb date/time values.
--
-- There is sufficient difficulty in dealing with Kdb date/time values that this
-- module was split out from @Database.Kdb.Internal.KdbTypes@. This difficulty
-- means that some conversions might be inefficient and could be improved.
--
-- In general, the time library stores dates and times to quick high precision
-- (picoseconds), therefore most conversions loose information.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.Types.DateTimeTypes (

    -- * Kdb epoch.
    -- $epoch
    kdbEpochDay
  , kdbEpochUTCTime

  -- * Timestamp type
    -- $timestamptype
  , KdbTimestampValue
  , toTimestamp
  , fromTimestamp
  , getTimestamp

  -- * Month type
    -- $monthtype
  , KdbMonthValue
  , toMonth
  , fromMonth
  , getMonth

    -- * Date type
    -- $datetype
  , KdbDateValue
  , toDate
  , fromDate
  , getDate

    -- * DateTime type
    -- $datetimetype
  , KdbDateTimeValue(..)
  , toDateTime
  , fromDateTime
  , getDateTime

    -- * Timespan type
    -- $timespantype
  , KdbTimespanValue(..)
  , toTimespan
  , fromTimespan
  , getTimespan

    -- * Minute type
    -- $minutetype
  , KdbMinuteValue(..)
  , toMinute
  , fromMinute
  , getMinute

    -- * Second type
    -- $secondtype
  , KdbSecondValue(..)
  , toSecond
  , fromSecond
  , getSecond

    -- * Time type
    -- $timetype
  , KdbTimeValue
  , toTime
  , fromTime
  , getTime
  ) where

import           Data.Fixed (Pico)
import           Data.Int   (Int32, Int64)
import qualified Data.Time  as Time

-- $epoch
--
-- Kdb epoch: 2000-01-01T00:00:00

-- | Year of the kdb epoch.
kdbYear :: Integer
kdbYear = 2000
{-# INLINE kdbYear #-}

-- | Month of the kdb epoch.
kdbMonth :: Int
kdbMonth = 1
{-# INLINE kdbMonth #-}

-- | Day of the kdb epoch
kdbDay :: Int
kdbDay = 1
{-# INLINE kdbDay #-}

-- | Start of Kdb epoch: 2000-01-01 in UTC.
kdbEpochDay :: Time.Day
kdbEpochDay = Time.fromGregorian kdbYear kdbMonth kdbDay
{-# INLINE kdbEpochDay #-}

kdbEpochUTCTime :: Time.UTCTime
kdbEpochUTCTime = Time.UTCTime kdbEpochDay (Time.timeOfDayToTime Time.midnight)
{-# INLINE kdbEpochUTCTime #-}

-- $timestamptype
--
-- Kdb Timestamp.

-- | Represents a Kdb timestamp value.
--
-- Kdb timestamp is the number of nanoseconds since the kdb epoch.
newtype KdbTimestampValue = KdbTimestampValue Int64
  deriving (Show, Integral, Real, Enum, Eq, Ord, Num)

-- | Converts @Time.UTCTime@ to @KdbTimestampValue@.
--
-- __Note:__ @Time.UTCTime@ is in pico seconds,
-- whereas @KdbTimestampValue@ is in nanoseconds.
toTimestamp :: Time.UTCTime -> KdbTimestampValue
toTimestamp utc =
  let offsetFromEpoch = Time.diffUTCTime utc kdbEpochUTCTime
  in KdbTimestampValue . floor $ offsetFromEpoch * nanosInSecond
{-# INLINEABLE toTimestamp #-}

-- | Converts @KdbTimestampValue@ to @Time.UTCTime@.
fromTimestamp :: KdbTimestampValue -> Time.UTCTime
fromTimestamp (KdbTimestampValue t) =
  let diffTime = fromIntegral t / nanosInSecond
  in Time.addUTCTime diffTime kdbEpochUTCTime
{-# INLINEABLE fromTimestamp #-}

-- | Unwraps the underlying value from @KdbTimestampValue@.
getTimestamp :: KdbTimestampValue -> Int64
getTimestamp (KdbTimestampValue d) = d
{-# INLINE getTimestamp #-}

-- $monthtype
--
-- Kdb Month.

-- | Represents a Kdb month value.
--
-- Kdb month is the number of months since the kdb epoch.
-- Can represent dates such as `2014-10`.
newtype KdbMonthValue = KdbMonthValue Int32
  deriving (Show, Integral, Real, Enum, Eq, Ord, Num)

-- | Converts @Time.Day@ to @KdbMonthValue@.
--
-- __Note:__ @Time.Day@ is the number of days,
-- whereas @KdbMonthValue@ is accurate to the month.
toMonth :: Time.Day -> KdbMonthValue
toMonth day =
  let (year, month, _) = Time.toGregorian day
  in KdbMonthValue .fromIntegral $ (year - kdbYear) * 12 + fromIntegral (month - 1)
{-# INLINEABLE toMonth #-}

-- | Converts @KdbMonthValue@ to @Time.Day@.
fromMonth :: KdbMonthValue -> Time.Day
fromMonth (KdbMonthValue m) =
  let year  = kdbYear + (fromIntegral m `div` 12)
      month = fromIntegral $ m `mod` 12 + 1
  in Time.fromGregorian year month 1
{-# INLINEABLE fromMonth #-}

-- | Unwraps the underlying value from @KdbMonthValue@.
getMonth :: KdbMonthValue -> Int32
getMonth (KdbMonthValue d) = d
{-# INLINE getMonth #-}

-- $datetype
--
-- Kdb date.

-- | Represents a Kdb date value.
--
-- Kdb date is number of days since @kdbEpochDay@. This value
-- can be negative to represent days before the epoch.
newtype KdbDateValue = KdbDateValue Int32
  deriving (Show, Integral, Real, Enum, Eq, Ord, Num)

-- | Converts @Time.Day@ to @KdbDateValue@.
toDate :: Time.Day -> KdbDateValue
toDate tDay = KdbDateValue . fromIntegral $ Time.diffDays tDay kdbEpochDay
{-# INLINEABLE toDate #-}

-- | Converts @KdbDateValue@ to @Time.Day@.
fromDate :: KdbDateValue -> Time.Day
fromDate (KdbDateValue d) = Time.addDays (fromIntegral d) kdbEpochDay
{-# INLINEABLE fromDate #-}

-- | Unwraps the underlying value from @KdbDateValue@.
getDate :: KdbDateValue -> Int32
getDate (KdbDateValue d) = d
{-# INLINE getDate #-}

-- $datetimetype
--
-- Kdb DateTime.

-- | Represents a Kdb datetime value.
--
-- Kdb datetime is the number of days since @kdbEpochDay@,
-- represented as @Double@.
newtype KdbDateTimeValue = KdbDateTimeValue Double
  deriving (Show, Real, Enum, Eq, Ord, Num)

-- | Converts @Time.UTCTime@ to @KdbDateTimeValue@.
--
-- __Note:__ @KdbDateTimeValue@ is represented as a double
-- in kdb, therefore:
-- @
--   x \= (fromDateTime . toDateTime $ x)
-- @
--
-- i.e. conversion to and from @KdbDateTimeValue@ looses information.
toDateTime :: Time.UTCTime -> KdbDateTimeValue
toDateTime utc =
  let offsetFromEpoch = Time.diffUTCTime utc kdbEpochUTCTime
  in KdbDateTimeValue $ realToFrac offsetFromEpoch / secsInDay
{-# INLINEABLE toDateTime #-}

-- | Converts @KdbDateTimeValue@ to @Time.UTCTime@.
fromDateTime :: KdbDateTimeValue -> Time.UTCTime
fromDateTime (KdbDateTimeValue t) =
  let diffTime = realToFrac (t * secsInDay)
  in Time.addUTCTime diffTime kdbEpochUTCTime
{-# INLINEABLE fromDateTime #-}

-- | Unwraps the underlying value from @KdbDateTimeValue@.
getDateTime :: KdbDateTimeValue -> Double
getDateTime (KdbDateTimeValue d) = d
{-# INLINE getDateTime #-}

-- $timespantype
--
-- Kdb timespan.

-- | Represents a Kdb timespan value.
--
-- Kdb timespan is the number of nanoseconds that represents
-- an amount of time.
newtype KdbTimespanValue = KdbTimespanValue Int64
  deriving (Show, Integral, Real, Enum, Eq, Ord, Num)

-- | Converts @Time.NominalDiffTime@ to @KdbTimespanValue@.
--
-- __Note:__ @Time.NominalDiffTime@ is in pico seconds,
-- whereas @KdbTimespanValue@ is in nanoseconds.
toTimespan :: Time.NominalDiffTime -> KdbTimespanValue
toTimespan td = KdbTimespanValue . floor $ td * nanosInSecond
{-# INLINEABLE toTimespan #-}

-- | Converts @KdbTimespanValue@ to @Time.NominalDiffTime@.
fromTimespan :: KdbTimespanValue -> Time.NominalDiffTime
fromTimespan (KdbTimespanValue t) = realToFrac t / nanosInSecond
{-# INLINEABLE fromTimespan #-}

-- | Unwraps the underlying value from @KdbTimespanValue@.
getTimespan :: KdbTimespanValue -> Int64
getTimespan (KdbTimespanValue t) = t
{-# INLINE getTimespan #-}

-- $minutetype
--
-- Kdb minute.

-- | Represents a Kdb minute value.
--
-- Kdb minute is the number of minutes that represents
-- an amount of time.
newtype KdbMinuteValue = KdbMinuteValue Int32
  deriving (Show, Integral, Real, Enum, Eq, Ord, Num)

-- | Converts @Time.NominalDiffTime@ to @KdbMinuteValue@.
--
-- __Note:__ @Time.NominalDiffTime@ is in pico seconds,
-- whereas @KdbMinuteValue@ is in minutes.
toMinute :: Time.NominalDiffTime -> KdbMinuteValue
toMinute td = KdbMinuteValue . floor $ td / 60
{-# INLINEABLE toMinute #-}

-- | Converts @KdbMinuteValue@ to @Time.NominalDiffTime@.
fromMinute :: KdbMinuteValue -> Time.NominalDiffTime
fromMinute (KdbMinuteValue t) = fromIntegral $ t * 60
{-# INLINEABLE fromMinute #-}

-- | Unwraps the underlying value from @KdbTimespanValue@.
getMinute :: KdbMinuteValue -> Int32
getMinute (KdbMinuteValue t) = t
{-# INLINE getMinute #-}

-- $minutetype
--
-- Kdb second.

-- | Represents a Kdb second value.
--
-- Kdb second is the number of seconds that represents
-- an amount of time.
newtype KdbSecondValue = KdbSecondValue Int32
  deriving (Show, Integral, Real, Enum, Eq, Ord, Num)

-- | Converts @Time.NominalDiffTime@ to @KdbSecondValue@.
--
-- __Note:__ @Time.NominalDiffTime@ is in pico seconds,
-- whereas @KdbSecondValue@ is in seconds.
toSecond :: Time.NominalDiffTime -> KdbSecondValue
toSecond td = KdbSecondValue . floor $ td
{-# INLINEABLE toSecond #-}

-- | Converts @KdSecondValue@ to @Time.NominalDiffTime@.
fromSecond :: KdbSecondValue -> Time.NominalDiffTime
fromSecond (KdbSecondValue t) = fromIntegral t
{-# INLINEABLE fromSecond #-}

-- | Unwraps the underlying value from @KdbTimespanValue@.
getSecond :: KdbSecondValue -> Int32
getSecond (KdbSecondValue t) = t
{-# INLINE getSecond #-}

-- $timetype
--
-- Kdb time.

-- | Represents a Kdb time value.
--
-- Kdb time is the number of milliseconds since midnight.
newtype KdbTimeValue = KdbTimeValue Int32
  deriving (Show, Integral, Real, Enum, Eq, Ord, Num)

-- | Converts @Time.TimeOfDay@ to @KdbTimeValue@.
--
-- @Time.TimeOfDay@ should be within a single day,
-- this invariant is not checked here.
--
-- __Note:__ @Time.TimeOfDay@ is in pico seconds,
-- whereas @KdbTimeValue@ is in milliseconds.
-- This means that in general:
-- @
--   x \= (fromTime . toTime $ x)
-- @
--
-- i.e. conversion to and from @KdbTimeValue@ looses information.
toTime :: Time.TimeOfDay -> KdbTimeValue
toTime (Time.TimeOfDay hs ms sec) =
  let hours    = hs  * millisInHour           -- hours in millis
      minutes  = ms  * millisInMinute         -- minutes in millis
      secs     = floor $ sec * millisInSecondPico -- seconds in millis
      sum'     = hours + minutes + secs
  in KdbTimeValue . fromIntegral $ sum'
{-# INLINEABLE toTime #-}

-- | Converts @KdbTimeValue@ to @Time.TimeOfDay@.
fromTime :: KdbTimeValue -> Time.TimeOfDay
fromTime (KdbTimeValue t) = Time.timeToTimeOfDay . Time.picosecondsToDiffTime $ fromIntegral t * millisInPico
{-# INLINEABLE fromTime #-}

-- | Unwraps the underlying value from @KdbTimeValue@.
getTime :: KdbTimeValue -> Int32
getTime (KdbTimeValue t) = t
{-# INLINE getTime #-}

-- Utility functions.

millisInHour :: Int
millisInHour = 60 * 60 * 1000
{-# INLINE millisInHour #-}

millisInMinute :: Int
millisInMinute = 60 * 1000
{-# INLINE millisInMinute #-}

millisInSecondPico :: Pico
millisInSecondPico = 1000
{-# INLINE millisInSecondPico #-}

nanosInSecond :: Time.NominalDiffTime
nanosInSecond = 1000000000
{-# INLINE nanosInSecond #-}

millisInPico :: Integer
millisInPico = 1000000000
{-# INLINE millisInPico #-}

secsInDay :: Double
secsInDay = 86400
{-# INLINE secsInDay #-}
