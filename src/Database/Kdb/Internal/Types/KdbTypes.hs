{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.KdbTypes
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Haskell types that represent Kdb+ types.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.Types.KdbTypes (
    -- * Data types
    -- $types
    Atom(..)
  , Vector(..)
  , Value(..)
    -- * K Types
    -- *ktypes
  , boolT
  , byteT
  , shortT
  , intT
  , longT
  , realT
  , floatT
  , charT
  , symT
  , timestampT
  , monthT
  , dateT
  , dateTimeT
  , timespanT
  , minuteT
  , secondT
  , timeT
  , boolVT
  , byteVT
  , shortVT
  , intVT
  , longVT
  , realVT
  , floatVT
  , charVT
  , symVT
  , timestampVT
  , monthVT
  , dateVT
  , dateTimeVT
  , timespanVT
  , minuteVT
  , secondVT
  , timeVT
  , tableT
  , listT
  , dictT
    -- * Utility functions
    -- $utils
  , size
  , qType
  , numElements
    -- * Constructors
    -- $constructors
  , bool
  , byte
  , short
  , int
  , long
  , real
  , float
  , char
  , s
  , timestamp
  , month
  , date
  , dateTime
  , timespan
  , minute
  , second
  , time
  , boolV
  , byteV
  , shortV
  , intV
  , intVV
  , longV
  , longVV
  , realV
  , floatV
  , charV
  , symV
  , symVV
  , timestampV
  , monthV
  , dateV
  , dateTimeV
  , timespanV
  , minuteV
  , secondV
  , timeV
  , table
  , table'
  , list
  , dict
    -- * Null values
    -- $nulls
  , nullBool
  , nullByte
  , nullShort
  , nullInt
  , nullLong
  , nullReal
  , nullFloat
  , nullChar
  , nullDate
  , nullTime
  , nullTimestamp
  , nullTimespan
  , nullDateTime
  ) where

import           Control.Monad                             (foldM_)
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString                           as B
import           Data.Int                                  (Int16, Int32, Int64,
                                                            Int8)
import           Data.List                                 (foldl')
import qualified Data.Time                                 as Time
import           Data.Typeable                             (Typeable)
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV
import           Data.Word
import qualified Database.Kdb.Internal.Types.DateTimeTypes as DateTime
import           Foreign                                   (Storable, sizeOf)
import           Foreign.C.String                          (castCharToCChar)
import           Foreign.C.Types                           (CChar)


-- DeepSeq and Generics
import           Control.DeepSeq                           (NFData (..))

-- Unsafe functions
import qualified Data.Vector.Storable.Mutable              as MSV
import           System.IO.Unsafe                          (unsafePerformIO)

-- $types
--
-- Some haddocks on the types.

-- | Kdb atom represented as Haskell value.
data Atom
      -- | Boolean atom
    = KBool      {-# UNPACK #-} !Word8
      -- | Byte atom
    | KByte      {-# UNPACK #-} !Word8
      -- | Short atom
    | KShort     {-# UNPACK #-} !Int16
      -- | Int atom
    | KInt       {-# UNPACK #-} !Int32
      -- | Long atom
    | KLong      {-# UNPACK #-} !Int64
      -- | Real atom
    | KReal      {-# UNPACK #-} !Float
      -- | Float atom
    | KFloat     {-# UNPACK #-} !Double
      -- | Character atom
    | KChar      {-# UNPACK #-} !CChar
      -- | Symbol - null terminated C string
    | KSym       {-# UNPACK #-} !(SV.Vector CChar)
      -- | Timestamp atom
    | KTimestamp {-# UNPACK #-} !Int64
      -- | Month atom
    | KMonth     {-# UNPACK #-} !Int32
      -- | Date atom
    | KDate      {-# UNPACK #-} !Int32
      -- | DateTime atom
    | KDateTime  {-# UNPACK #-} !Double
      -- | Timespan atom
    | KTimespan  {-# UNPACK #-} !Int64
      -- | Minute atom
    | KMinute    {-# UNPACK #-} !Int32
      -- | Second atom
    | KSecond    {-# UNPACK #-} !Int32
      -- | Time atom
    | KTime      {-# UNPACK #-} !Int32
  deriving (Eq, Show, Typeable)

instance NFData Atom where
    rnf (KBool      x) = rnf x
    rnf (KByte      x) = rnf x
    rnf (KShort     x) = rnf x
    rnf (KInt       x) = rnf x
    rnf (KLong      x) = rnf x
    rnf (KReal      x) = rnf x
    rnf (KFloat     x) = rnf x
    rnf (KChar      x) = x `seq` ()
    rnf (KSym       x) = rnf x
    rnf (KTimestamp x) = rnf x
    rnf (KMonth     x) = rnf x
    rnf (KDate      x) = rnf x
    rnf (KDateTime  x) = rnf x
    rnf (KTimespan  x) = rnf x
    rnf (KMinute    x) = rnf x
    rnf (KSecond    x) = rnf x
    rnf (KTime      x) = rnf x

-- | Kdb vector represented as Haskell value.
data Vector
      -- | Boolean vector
    = KBoolV      {-# UNPACK #-} !(SV.Vector Word8)
      -- | Byte vector
    | KByteV      {-# UNPACK #-} !(SV.Vector Word8)
      -- | Short vector
    | KShortV     {-# UNPACK #-} !(SV.Vector Int16)
      -- | Int vector
    | KIntV       {-# UNPACK #-} !(SV.Vector Int32)
      -- | Long vector
    | KLongV      {-# UNPACK #-} !(SV.Vector Int64)
      -- | Real vector
    | KRealV      {-# UNPACK #-} !(SV.Vector Float)
      -- | Float vector
    | KFloatV     {-# UNPACK #-} !(SV.Vector Double)
      -- | Character vector: just a list of chars, no need to store length or total bytes
    | KCharV      {-# UNPACK #-} !(SV.Vector CChar)
      -- | Symbol vector: Int stores the number of null-terminated C Strings
    | KSymV       {-# UNPACK #-}  !Int {-# UNPACK #-} !(SV.Vector CChar)
      -- | Timestamp vector
    | KTimestampV {-# UNPACK #-} !(SV.Vector Int64)
      -- | Month vector
    | KMonthV     {-# UNPACK #-} !(SV.Vector Int32)
      -- | Date vector
    | KDateV      {-# UNPACK #-} !(SV.Vector Int32)
      -- | DateTime vector
    | KDateTimeV  {-# UNPACK #-} !(SV.Vector Double)
      -- | Timespan vector
    | KTimespanV  {-# UNPACK #-} !(SV.Vector Int64)
      -- | Minute vector
    | KMinuteV    {-# UNPACK #-} !(SV.Vector Int32)
      -- | Second vector
    | KSecondV    {-# UNPACK #-} !(SV.Vector Int32)
      -- | Time vector
    | KTimeV      {-# UNPACK #-} !(SV.Vector Int32)
  deriving (Eq, Show, Typeable)

instance NFData Vector where
    rnf (KBoolV        x) = rnf x
    rnf (KByteV        x) = rnf x
    rnf (KShortV       x) = rnf x
    rnf (KIntV         x) = rnf x
    rnf (KLongV        x) = rnf x
    rnf (KRealV        x) = rnf x
    rnf (KFloatV       x) = rnf x
    rnf (KCharV        x) = rnf x
    rnf (KSymV       x y) = rnf x `seq` rnf y
    rnf (KTimestampV   x) = rnf x
    rnf (KMonthV       x) = rnf x
    rnf (KDateV        x) = rnf x
    rnf (KDateTimeV    x) = rnf x
    rnf (KTimespanV    x) = rnf x
    rnf (KMinuteV      x) = rnf x
    rnf (KSecondV      x) = rnf x
    rnf (KTimeV        x) = rnf x

-- | Kdb value represented as Haskell value.
data Value
      -- | Atom value.
    = A   !Atom
      -- | Vector value.
    | V   !Vector
      -- | Untyped vector: e.g. remote function call of the form ("insert";`t;table)
    | KList  {-# UNPACK #-} !(V.Vector Value)
      -- | Dictionary: e.g. `a`b!2 3
    | KDict  {-# UNPACK #-} !Vector {-# UNPACK #-} !Vector
      -- | Q table: Int stores total bytes needed to encode table in `Vector`
    | KTable {-# UNPACK #-} !Int {-# UNPACK #-} !(V.Vector Value)
  deriving (Eq, Show, Typeable)

instance NFData Value where
    rnf (A        x)  = rnf x
    rnf (V        x)  = rnf x
    rnf (KList    x)  = rnf x
    rnf (KDict  x y)  = rnf x `seq` rnf y
    rnf (KTable x y)  = rnf x `seq` rnf y

-- $ktypes
--
-- @Word8@ values for kdb types and a function that maps between Haskell types and @Word8@ types.

-- | Kdb @Word8@ type for Bool atom.
boolT :: Word8
boolT = -1
{-# INLINE boolT #-}

-- | Kdb @Word8@ value for Byte atom.
byteT :: Word8
byteT = -4
{-# INLINE byteT #-}

-- | Kdb @Word8@ value for Short atom.
shortT :: Word8
shortT = -5
{-# INLINE shortT #-}

-- | Kdb @Word8@ value for Integer atom.
intT :: Word8
intT = -6
{-# INLINE intT #-}

-- | Kdb @Word8@ value for Long atom.
longT :: Word8
longT = -7
{-# INLINE longT #-}

-- | Kdb @Word8@ value for Real atom.
realT :: Word8
realT = -8
{-# INLINE realT #-}

-- | Kdb @Word8@ value for Float atom.
floatT :: Word8
floatT = -9
{-# INLINE floatT #-}

-- | Kdb @Word8@ value for Char atom.
charT :: Word8
charT = -10
{-# INLINE charT #-}

-- | Kdb @Word8@ value for Symbol atom.
symT :: Word8
symT = -11
{-# INLINE symT #-}

-- | Kdb @Word8@ value for Timestamp atom.
timestampT :: Word8
timestampT = -12
{-# INLINE timestampT #-}

-- | Kdb @Word8@ value for Month atom.
monthT :: Word8
monthT = -13
{-# INLINE monthT #-}

-- | Kdb @Word8@ value for Date atom.
dateT :: Word8
dateT = -14
{-# INLINE dateT #-}

-- | Kdb @Word8@ value for DateTime atom.
dateTimeT :: Word8
dateTimeT = -15
{-# INLINE dateTimeT #-}

-- | Kdb @Word8@ value for Timespan atom.
timespanT :: Word8
timespanT = -16
{-# INLINE timespanT #-}

-- | Kdb @Word8@ value for Minute atom.
minuteT :: Word8
minuteT = -17
{-# INLINE minuteT #-}

-- | Kdb @Word8@ value for Second atom.
secondT :: Word8
secondT = -18
{-# INLINE secondT #-}

-- | Kdb @Word8@ value for Time atom.
timeT :: Word8
timeT = -19
{-# INLINE timeT #-}

-- | Kdb @Word8@ value for Boolean vector.
boolVT :: Word8
boolVT = negate boolT
{-# INLINE boolVT #-}

-- | Kdb @Word8@ value for Byte vector.
byteVT :: Word8
byteVT = negate byteT
{-# INLINE byteVT #-}

-- | Kdb @Word8@ value for Short vector.
shortVT :: Word8
shortVT = negate shortT
{-# INLINE shortVT #-}

-- | Kdb @Word8@ value for Integer vector.
intVT :: Word8
intVT = negate intT
{-# INLINE intVT #-}

-- | Kdb @Word8@ value for Long vector.
longVT :: Word8
longVT = negate longT
{-# INLINE longVT #-}

-- | Kdb @Word8@ value for Real vector.
realVT :: Word8
realVT = negate realT
{-# INLINE realVT #-}

-- | Kdb @Word8@ value for Float vector.
floatVT :: Word8
floatVT = negate floatT
{-# INLINE floatVT #-}

-- | Kdb @Word8@ value for Char vector.
charVT :: Word8
charVT = negate charT
{-# INLINE charVT #-}

-- | Kdb @Word8@ value for Symbol vector.
symVT :: Word8
symVT = negate symT
{-# INLINE symVT #-}

-- | Kdb @Word8@ value for Timestamp vector.
timestampVT :: Word8
timestampVT = negate timestampT
{-# INLINE timestampVT #-}

-- | Kdb @Word8@ value for Month vector.
monthVT :: Word8
monthVT = negate monthT
{-# INLINE monthVT #-}

-- | Kdb @Word8@ value for Date vector.
dateVT :: Word8
dateVT = negate dateT
{-# INLINE dateVT #-}

-- | Kdb @Word8@ value for DateTime vector.
dateTimeVT :: Word8
dateTimeVT = negate dateTimeT
{-# INLINE dateTimeVT #-}

-- | Kdb @Word8@ value for Timespan vector.
timespanVT :: Word8
timespanVT = negate timespanT
{-# INLINE timespanVT #-}

-- | Kdb @Word8@ value for Minute vector.
minuteVT :: Word8
minuteVT = negate minuteT
{-# INLINE minuteVT #-}

-- | Kdb @Word8@ value for Second vector.
secondVT :: Word8
secondVT = negate secondT
{-# INLINE secondVT #-}

-- | Kdb @Word8@ value for Time vector.
timeVT :: Word8
timeVT = negate timeT
{-# INLINE timeVT #-}

-- | Kdb @Word8@ value for a Table.
tableT :: Word8
tableT = 98
{-# INLINE tableT #-}

-- | Kdb @Word8@ value for a List.
listT :: Word8
listT = 0
{-# INLINE listT #-}

-- | Kdb @Word8@ value for a Dictionary.
dictT :: Word8
dictT = 99
{-# INLINE dictT #-}

-- | Gets q type of the object.
qType :: Value -> Int8
qType (A (KBool         _)) = fromIntegral boolT
qType (A (KByte         _)) = fromIntegral byteT
qType (A (KShort        _)) = fromIntegral shortT
qType (A (KInt          _)) = fromIntegral intT
qType (A (KLong         _)) = fromIntegral longT
qType (A (KReal         _)) = fromIntegral realT
qType (A (KFloat        _)) = fromIntegral floatT
qType (A (KChar         _)) = fromIntegral charT
qType (A (KSym          _)) = fromIntegral symT
qType (A (KTimestamp    _)) = fromIntegral timestampT
qType (A (KMonth        _)) = fromIntegral monthT
qType (A (KDate         _)) = fromIntegral dateT
qType (A (KDateTime     _)) = fromIntegral dateTimeT
qType (A (KTimespan     _)) = fromIntegral timespanT
qType (A (KMinute       _)) = fromIntegral minuteT
qType (A (KSecond       _)) = fromIntegral secondT
qType (A (KTime         _)) = fromIntegral timeT
qType (V (KBoolV        _)) = fromIntegral boolVT
qType (V (KByteV        _)) = fromIntegral byteVT
qType (V (KShortV       _)) = fromIntegral shortVT
qType (V (KIntV         _)) = fromIntegral intVT
qType (V (KLongV        _)) = fromIntegral longVT
qType (V (KRealV        _)) = fromIntegral realVT
qType (V (KFloatV       _)) = fromIntegral floatVT
qType (V (KCharV        _)) = fromIntegral charVT
qType (V (KSymV       _ _)) = fromIntegral symVT
qType (V (KTimestampV   _)) = fromIntegral timestampVT
qType (V (KMonthV       _)) = fromIntegral monthVT
qType (V (KDateV        _)) = fromIntegral dateVT
qType (V (KDateTimeV    _)) = fromIntegral dateTimeVT
qType (V (KTimespanV    _)) = fromIntegral timespanVT
qType (V (KMinuteV      _)) = fromIntegral minuteVT
qType (V (KSecondV      _)) = fromIntegral secondVT
qType (V (KTimeV        _)) = fromIntegral timeVT
qType (KTable         _ _)  = fromIntegral tableT
qType (KList            _)  = fromIntegral listT
qType (KDict          _ _)  = fromIntegral dictT
{-# INLINE qType #-}

-- $utils
--
-- Some haddocks on the utility functions.

-- | Size of the atom header.
atomSize :: Storable a => a ->  Int
atomSize a = 1 + sizeOf a
{-# INLINE atomSize #-}

-- | Size of the vector header.
vectorSize :: (Storable a) => a -> SV.Vector a -> Int
vectorSize a v = 6 + sizeOf a * SV.length v
{-# INLINE vectorSize #-}

-- | Gets total size in bytes taken to store the data from `Value`.
--
-- Used to calculate total bytes needed to build the ByteString for IPC to Q server
size :: Value -> Int
size (A (KBool       x)) = atomSize x
size (A (KByte       x)) = atomSize x
size (A (KShort      x)) = atomSize x
size (A (KInt        x)) = atomSize x
size (A (KLong       x)) = atomSize x
size (A (KReal       x)) = atomSize x
size (A (KFloat      x)) = atomSize x
size (A (KChar       x)) = atomSize x
size (A (KSym        x)) = 1 + sizeOf (undefined :: CChar) * SV.length x
size (A (KTimestamp  x)) = atomSize x
size (A (KMonth      x)) = atomSize x
size (A (KDate       x)) = atomSize x
size (A (KDateTime   x)) = atomSize x
size (A (KTimespan   x)) = atomSize x
size (A (KMinute     x)) = atomSize x
size (A (KSecond     x)) = atomSize x
size (A (KTime       x)) = atomSize x
size (V (KBoolV      x)) = vectorSize (undefined :: Word8) x
size (V (KByteV      x)) = vectorSize (undefined :: Word8) x
size (V (KShortV     x)) = vectorSize (undefined :: Int16) x
size (V (KIntV       x)) = vectorSize (undefined :: Int32) x
size (V (KLongV      x)) = vectorSize (undefined :: Int64) x
size (V (KRealV      x)) = vectorSize (undefined :: Float) x
size (V (KFloatV     x)) = vectorSize (undefined :: Double) x
size (V (KCharV      x)) = vectorSize (undefined :: CChar) x
size (V (KSymV     _ x)) = vectorSize (undefined :: CChar) x
size (V (KTimestampV x)) = vectorSize (undefined :: Int64) x
size (V (KMonthV     x)) = vectorSize (undefined :: Int32) x
size (V (KDateV      x)) = vectorSize (undefined :: Int32) x
size (V (KDateTimeV  x)) = vectorSize (undefined :: Double) x
size (V (KTimespanV  x)) = vectorSize (undefined :: Int64) x
size (V (KMinuteV    x)) = vectorSize (undefined :: Int32) x
size (V (KSecondV    x)) = vectorSize (undefined :: Int32) x
size (V (KTimeV      x)) = vectorSize (undefined :: Int32) x
size (KTable       n _)  = n
size (KList          x)  = V.foldl' (\x' y -> x' + size y) 6 x
size (KDict        l r)  = 1 + (size . V $ l) + (size . V $ r)
{-# INLINE size #-}

numElements :: Value -> Int
numElements (V (KBoolV      x)) = SV.length x
numElements (V (KByteV      x)) = SV.length x
numElements (V (KShortV     x)) = SV.length x
numElements (V (KIntV       x)) = SV.length x
numElements (V (KLongV      x)) = SV.length x
numElements (V (KRealV      x)) = SV.length x
numElements (V (KFloatV     x)) = SV.length x
numElements (V (KCharV      x)) = SV.length x
numElements (V (KSymV     n _)) = n
numElements (V (KTimestampV x)) = SV.length x
numElements (V (KMonthV     x)) = SV.length x
numElements (V (KDateV      x)) = SV.length x
numElements (V (KDateTimeV  x)) = SV.length x
numElements (V (KTimespanV  x)) = SV.length x
numElements (V (KMinuteV    x)) = SV.length x
numElements (V (KSecondV    x)) = SV.length x
numElements (V (KTimeV      x)) = SV.length x
numElements (KList          x)  = V.length x
numElements (A              _)  = undefined
numElements (KDict        _ _)  = undefined
numElements (KTable       _ _)  = undefined
{-# INLINE numElements #-}

-- $constructors
--
-- Constructor functions for creating atoms and vectors.

toBool :: Bool -> Word8
toBool False = 0
toBool True  = 1
{-# INLINE toBool #-}

-- | Creates a Bool atom.
bool :: Bool -> Value
bool = A . KBool . toBool
{-# INLINEABLE bool #-}

-- | Creates a Byte atom.
byte :: Word8 -> Value
byte = A . KByte
{-# INLINEABLE byte #-}

-- | Creates a Short atom.
short :: Int16 -> Value
short = A . KShort
{-# INLINEABLE short #-}

-- | Creates an Integer atom.
int :: Int -> Value
int = A . KInt . fromIntegral
{-# INLINEABLE int #-}

-- | Creates a Long atom.
long :: Integral a => a -> Value
long = A . KLong . fromIntegral
{-# INLINEABLE long #-}

-- | Creates a Real atom.
real :: Float -> Value
real = A . KReal
{-# INLINEABLE real #-}

-- | Creates a Float atom.
float :: Double -> Value
float = A . KFloat
{-# INLINEABLE float #-}

-- | Creates a Char atom.
--
-- This function is only safe on the first 256 characters.
char :: Char -> Value
char = A . KChar . castCharToCChar
{-# INLINEABLE char #-}

-- | Creates null-terminated Symbol from `ByteString`.
--
-- The given `ByteString` should not have null-bytes.
s :: ByteString -> Value
s !bs = A . KSym $ unsafePerformIO $ do
    v <- MSV.new fl
    _ <- fill0 v 0 0 bs
    SV.unsafeFreeze v
  where
    fl = B.length bs + 1 -- '\0' terminated string
{-# INLINEABLE s #-}

-- | Creates a Timestamp atom.
timestamp :: Time.UTCTime -> Value
timestamp = A . KTimestamp . DateTime.getTimestamp . DateTime.toTimestamp
{-# INLINEABLE timestamp #-}

-- | Creates a Month atom.
month :: Time.Day -> Value
month = A . KMonth . DateTime.getMonth . DateTime.toMonth
{-# INLINEABLE month #-}

-- | Creates a Date atom.
date :: Time.Day -> Value
date = A . KDate . DateTime.getDate . DateTime.toDate
{-# INLINEABLE date #-}

-- | Creates a DateTime atom.
dateTime :: Time.UTCTime -> Value
dateTime = A . KDateTime . DateTime.getDateTime . DateTime.toDateTime
{-# INLINEABLE dateTime #-}

-- | Creates a Timespan atom.
timespan :: Time.NominalDiffTime -> Value
timespan = A . KTimespan . DateTime.getTimespan . DateTime.toTimespan
{-# INLINEABLE timespan #-}

-- | Creates a Minute atom.
minute :: Time.NominalDiffTime -> Value
minute = A . KMinute . DateTime.getMinute . DateTime.toMinute
{-# INLINEABLE minute #-}

-- | Creates a Second atom.
second :: Time.NominalDiffTime -> Value
second = A . KSecond . DateTime.getSecond . DateTime.toSecond
{-# INLINEABLE second #-}

-- | Creates a Time atom.
time :: Time.TimeOfDay -> Value
time = A . KTime . DateTime.getTime . DateTime.toTime
{-# INLINEABLE time #-}

-- | Creates a Bool vector.
boolV :: [Bool] -> Value
boolV = V . KBoolV . SV.fromList . map toBool
{-# INLINEABLE boolV #-}

-- | Creates a Byte vector.
byteV :: [Word8] -> Value
byteV = V . KByteV . SV.fromList
{-# INLINEABLE byteV #-}

-- | Creates a Short vector.
shortV :: [Int16] -> Value
shortV = V . KShortV . SV.fromList
{-# INLINEABLE shortV #-}

-- | Creates an Int vector value.
intV :: [Int32] -> Value
intV = V . KIntV . SV.fromList
{-# INLINEABLE intV #-}

-- | Creates an Int Vector.
intVV :: [Int] -> Vector
intVV = KIntV . SV.fromList . map fromIntegral
{-# INLINEABLE intVV #-}

-- | Creates a Long vector value.
longV :: [Int64] -> Value
longV = V . longVV
{-# INLINEABLE longV #-}

-- | Creates a Long vector.
longVV :: [Int64] -> Vector
longVV = KLongV . SV.fromList
{-# INLINEABLE longVV #-}

-- | Creates a Real vector value.
realV :: [Float] -> Value
realV = V . KRealV . SV.fromList
{-# INLINEABLE realV #-}

-- | Creates a Float vector.
floatV :: [Double] -> Value
floatV = V . KFloatV . SV.fromList
{-# INLINEABLE floatV #-}

-- | Creates a Char vector from @ByteString@.
charV :: ByteString -> Value
charV !bs = V . KCharV $ SV.generate (B.length bs) (fromIntegral . B.index bs)
{-# INLINEABLE charV #-}

-- | Creates Symbol vector value from a list of @ByteString@.
symV :: [ByteString] -> Value
symV = V . symVV
{-# INLINEABLE symV #-}

-- | Creates @KSymV@ from a list of @ByteString@.
symVV :: [ByteString] -> Vector
symVV bss = KSymV bssl $ unsafePerformIO $ do
    v <- MSV.new sl
    foldM_ (fill0 v 0) 0 bss
    SV.unsafeFreeze v
  where sl = foldl' (\b a -> b + B.length a + 1) 0 bss
        bssl = length bss
{-# INLINEABLE symVV #-}

-- | Creates a Timestamp vector.
timestampV :: [Time.UTCTime] -> Value
timestampV = V . KTimestampV . SV.fromList . map (DateTime.getTimestamp . DateTime.toTimestamp)
{-# INLINEABLE timestampV #-}

-- | Creates a Month vector.
monthV :: [Time.Day] -> Value
monthV = V . KMonthV . SV.fromList . map (DateTime.getMonth . DateTime.toMonth)
{-# INLINEABLE monthV #-}

-- | Creates a Date vector.
dateV :: [Time.Day] -> Value
dateV = V . KDateV . SV.fromList . map (DateTime.getDate . DateTime.toDate)
{-# INLINEABLE dateV #-}

-- | Creates a DateTime vector.
dateTimeV :: [Time.UTCTime] -> Value
dateTimeV = V . KDateTimeV . SV.fromList . map (DateTime.getDateTime . DateTime.toDateTime)
{-# INLINEABLE dateTimeV #-}

-- | Creates a Timespan vector.
timespanV :: [Time.NominalDiffTime] -> Value
timespanV = V . KTimespanV . SV.fromList . map (DateTime.getTimespan . DateTime.toTimespan)
{-# INLINEABLE timespanV #-}

-- | Creates a Minute vector.
minuteV :: [Time.NominalDiffTime] -> Value
minuteV = V . KMinuteV . SV.fromList . map (DateTime.getMinute . DateTime.toMinute)
{-# INLINEABLE minuteV #-}

-- | Creates a Second vector.
secondV :: [Time.NominalDiffTime] -> Value
secondV = V . KSecondV . SV.fromList . map (DateTime.getSecond . DateTime.toSecond)
{-# INLINEABLE secondV #-}

-- | Creates a Second vector.
timeV :: [Time.TimeOfDay] -> Value
timeV = V . KTimeV . SV.fromList . map (DateTime.getTime . DateTime.toTime)
{-# INLINEABLE timeV #-}

-- | Creates @KList@ from a list of @Value@s.
list :: [Value] -> Value
list !xs = KList $ V.fromList xs
{-# INLINEABLE list #-}

-- | Creates a @KDict@ from a pair of @Value@s.
dict :: Vector -> Vector -> Value
dict = KDict
{-# INLINEABLE dict #-}

-- | Constructor for T - a Q table - we must always build it using this function
-- 2 bytes for table header - 1 additional byte for dict type header
table :: [Value] -> Value
table !xs = KTable (foldl' (\x y -> x + size y) 3 xs) (V.fromList xs)
{-# INLINEABLE table #-}

-- | Contructor for a @KTable@ from a vector of values.
table' :: V.Vector Value -> Value
table' !xs = KTable (V.foldl' (\x y -> x + size y) 3 xs) xs
{-# INLINEABLE table' #-}

-- $constructors
--
-- Null values for atoms and vectors.
--
-- __Note:__ Null values in kdb represent *technically*
-- correct values from the domain of the underlying types.
-- Therefore, they represent null values by convention.

-- | Null value for Boolean atom.
nullBool :: Value
nullBool = A . KBool . toBool $ False
{-# INLINEABLE nullBool #-}

-- | Null value for Byte atom.
nullByte :: Value
nullByte = A (KByte 0)
{-# INLINEABLE nullByte #-}

-- | Null value for Short atom.
nullShort :: Value
nullShort = A (KShort minBound)
{-# INLINEABLE nullShort #-}

-- | Null value for Int atom.
nullInt :: Value
nullInt = A (KInt minBound)
{-# INLINEABLE nullInt #-}

-- | Null value for Long atom.
nullLong :: Value
nullLong = A (KLong minBound)
{-# INLINEABLE nullLong #-}

-- | Null value for Real atom.
nullReal :: Value
nullReal = A . KReal $ 0/0
{-# INLINEABLE nullReal #-}

-- | Null value for Float atom.
nullFloat :: Value
nullFloat = A . KFloat $ 0/0
{-# INLINEABLE nullFloat #-}

-- | Null value for Char atom.
nullChar :: Value
nullChar = A . KChar . castCharToCChar $ ' '
{-# INLINEABLE nullChar #-}

-- | Null value for Date atom.
nullDate :: Value
nullDate = A (KDate minBound)
{-# INLINEABLE nullDate #-}

-- | Null value for Time atom.
nullTime :: Value
nullTime = A (KTime minBound)
{-# INLINEABLE nullTime #-}

-- | Null value for Timestamp atom.
nullTimestamp :: Value
nullTimestamp = A (KTimestamp minBound)
{-# INLINEABLE nullTimestamp #-}

-- | Null value for Timespan atom.
nullTimespan :: Value
nullTimespan = A (KTimespan minBound)
{-# INLINEABLE nullTimespan #-}

-- | Null value for DateTime atom.
nullDateTime :: Value
nullDateTime = A (KDateTime 0xfff8000000000000)
{-# INLINEABLE nullDateTime #-}

-- Private utility functions.

-- | Fills this `MSV.MVector` with this `ByteString` terminating with '\0' byte
-- and returns the next index to the vector.
--
-- This function assumes there is enough space in the vector and is in this shape
-- so that it is simple to pass to `foldM_`.
fill0 :: MSV.IOVector CChar -- Vector to fill
      -> Int                -- Current index into the `ByteString`
      -> Int                -- Current index into the vector
      -> ByteString         -- `ByteString` to write
      -> IO Int             -- Next index in the vector
fill0 v !bi' !vi' bs | B.length bs == bi' = MSV.unsafeWrite v vi' (0 :: CChar) >> return (vi' + 1)
fill0 v !bi' !vi' bs                      = MSV.unsafeWrite v vi' (fromIntegral $ B.index bs bi') >> fill0 v (bi' + 1) (vi' + 1) bs
