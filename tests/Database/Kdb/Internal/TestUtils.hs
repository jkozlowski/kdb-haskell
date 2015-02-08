{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.TestUtils
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Contains a test schema and random generators for values,
-- as well as utility functions.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.TestUtils (
    -- * Test Schema
    tableName, schema, randomRow, randomRows

    -- * Date/Time utils
  , secsWithPrecision, utc, utcTimeInScale, todsInScale, dayInScale
  , nominalDiffTimeWithPrecision, (@~=), DayPrecision(..), TimePrecision(..)

    -- * @C.ByteString@ utils
  , bprint

    -- $network
    -- Network utils
  , findFreePort

    -- $kdbutils
    -- * Kdb utils
  , kdbConnection, startKdb, user, pass

    -- $general
    -- * General utils
  , assertException
  ) where

import           Control.Applicative                     (pure, (<$>), (<*>))
import           Control.Arrow                           ((***))
import           Control.Lens
import           Control.Monad                           (guard, void)
import           Control.Monad.Catch                     (Exception,
                                                          Handler (..),
                                                          MonadCatch)
import qualified Control.Monad.Catch                     as E
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Resource
import           Control.Retry                           (constantDelay,
                                                          limitRetries,
                                                          recovering)
import           Data.ByteString                         (ByteString)
import qualified Data.ByteString.Char8                   as C
import           Data.Default.Class
import           Data.Fixed                              (Pico)
import           Data.Monoid                             ((<>))
import           Data.Ratio
import           Data.String                             (fromString)
import qualified Data.Time                               as Time
import qualified Database.Kdb.Internal.Client            as Client
import           Database.Kdb.Internal.Types.ClientTypes (Connection, password,
                                                          port, username)
import           Database.Kdb.Internal.Types.KdbTypes
import           Filesystem.Path.CurrentOS               (encodeString, (</>))
import           Network.Socket                          hiding (recv)
import           System.Directory                        (getCurrentDirectory,
                                                          getPermissions,
                                                          setOwnerExecutable,
                                                          setPermissions)
import           System.Info                             (os)
import           System.IO
import           System.IO.Error                         (catchIOError)
import qualified System.Process                          as Process
import           Test.QuickCheck                         (Gen, arbitrary,
                                                          choose, listOf1,
                                                          suchThat, vectorOf)
import           Test.QuickCheck.Gen                     (sample')
import           Test.Tasty.HUnit                        (Assertion,
                                                          assertFailure, (@?=))
import           Text.Printf                             (printf)

insert :: Value
insert = charV "insert"

tableName :: C.ByteString
tableName = "test"

testTableSym :: Value
testTableSym = s tableName

-- | This needs to be a symbol list eventually.
columns :: [C.ByteString]
columns = [
    "kbool"
  , "kbyte"
  , "kshort"
  , "kint"
  , "klong"
  , "kreal"
  , "kfloat"
  , "kchar"
  , "ksym"
  , "ktimestamp"
  , "kmonth"
  , "kdate"
  , "kdatetime"
  , "ktimespan"
  , "kminute"
  , "ksecond"
  , "ktime"
  ]

schema :: C.ByteString
schema = tableName <> ":([]" <> C.intercalate ";" (map withBraces columns) <> ")"
  where withBraces col = col `C.append` ":()"

-- Each row needs to be a list of (tableName, "insert", (vector of vectors))
randomRow :: Gen Value
randomRow = do
    kbool      <- boolV      <$> vectorOf 1 arbitrary
    kbyte      <- byteV      <$> vectorOf 1 arbitrary
    kshort     <- shortV     <$> vectorOf 1 arbitrary
    kint       <- intV       <$> vectorOf 1 arbitrary
    klong      <- longV      <$> vectorOf 1 arbitrary
    kreal      <- realV      <$> vectorOf 1 arbitrary
    kfloat     <- floatV     <$> vectorOf 1 arbitrary
    kchar      <- charV      <$> (C.pack <$> vectorOf 1 arbitrary)
    ksym       <- symV       <$> vectorOf 1 bstring
    ktimestamp <- timestampV <$> vectorOf 1 (utcTimeInScale NanoP)
    kmonth     <- monthV     <$> vectorOf 1 (dayInScale MonthP)
    kdate      <- dateV      <$> vectorOf 1 (dayInScale DayP)
    kdatetime  <- dateTimeV  <$> vectorOf 1 (utcTimeInScale MilliP)
    ktimespan  <- timespanV  <$> vectorOf 1 (nominalDiffTimeWithPrecision NanoP)
    kminute    <- minuteV    <$> vectorOf 1 (nominalDiffTimeWithPrecision MinuteP)
    ksecond    <- secondV    <$> vectorOf 1 (nominalDiffTimeWithPrecision SecondP)
    ktime      <- timeV      <$> vectorOf 1 (todsInScale MilliP)
    let values = list [
              kbool, kbyte, kshort
            , kint, klong, kreal
            , kfloat, kchar, ksym
            , ktimestamp, kmonth, kdate, kdatetime, ktimespan, kminute, ksecond, ktime
            ]
    return $! list [insert, testTableSym, values]
  where bstring = C.pack <$> listOf1 (suchThat (choose ('a', 'z')) notNulls)
        notNulls = (/=) '\0'

-- | Annoying to write...
randomRows :: Int -> IO [Value]
randomRows count = go 0
  where go cur = do
          vals <- sample' randomRow
          let l = length vals
          if cur + l >= count
           then return $ take (count - cur) vals
           else (vals ++) <$> go (cur + l)

utc :: Integer -> Int -> Int -> Int -> Int -> Pico -> Time.UTCTime
utc y month' d h minute' s' =
 let day  = Time.fromGregorian y month' d
     diffTime = Time.timeOfDayToTime $ Time.TimeOfDay h minute' s'
 in Time.UTCTime day diffTime

infix  1 @~=

-- | Compares @UTCTime@ up to the hour.
(@~=) :: Time.UTCTime -> Time.UTCTime -> Assertion
(@~=) utc1 utc2 = do
  Time.utctDay utc1 @?= Time.utctDay utc2
  let time1 = Time.timeToTimeOfDay $ Time.utctDayTime utc1
      time2 = Time.timeToTimeOfDay $ Time.utctDayTime utc2
  Time.todHour time1 @?= Time.todHour time2

-- | Precision for functions that generate
-- values that include time.
data TimePrecision = MinuteP
                   | SecondP
                   | MilliP
                   | NanoP

-- | Generates numbers between (0, max) with `scale` precision.
picoWithPrecision :: (Fractional a, Real a, RealFrac b) => Int -> a -> Gen b
picoWithPrecision max' scale' = (\x -> realToFrac (fromIntegral x / scale')) <$> choose (0, max')

-- (fromInteger $ round $ f * (10^n)) / (10.0^^n)

-- | Gets the scale for this @TimePrecision@ in seconds.
scale :: TimePrecision -> Rational
scale p = case p of
           MinuteP  -> 1 % 60
           SecondP -> 1 % 1
           MilliP  -> 1000 % 1
           NanoP   -> 1000000000 % 1

-- | Generates numbers between (0,60) inclusive with the given precision.
secsWithPrecision :: TimePrecision -> Gen Pico
secsWithPrecision p = picoWithPrecision 60 (scale p)

-- | Generates @Time.NominalDiffTime@ with the given precision up to
-- 10000 seconds, which is roughly 2.7 hours.
nominalDiffTimeWithPrecision :: TimePrecision -> Gen Time.NominalDiffTime
nominalDiffTimeWithPrecision p = picoWithPrecision 10000 (scale p)

-- | Generates @Time.TimeOfDay@ values with `scale` precision.
--
-- >> todsInScale MilliP
-- 08:51:00.233
--
-- >> todsInScale NanoP
-- 07:43:00.552285208
todsInScale :: TimePrecision -> Gen Time.TimeOfDay
todsInScale p =
  Time.TimeOfDay <$>
     choose (0, 23) <*>
     choose (0, 59) <*>
     secsWithPrecision p

-- | Generates @Time.UTCTime@ values with `scale` precision.
--
-- >> utcTimeInScale NanoP
-- 1886-03-10 08:41:00.641633206 UTC
utcTimeInScale :: TimePrecision -> Gen Time.UTCTime
utcTimeInScale p =
  let day = Time.fromGregorian        <$> choose (1858, 2050) <*> choose (1, 12) <*> choose (1, 31)
      diffTime = Time.timeOfDayToTime <$> todsInScale p
  in  Time.UTCTime <$>
      day          <*>
      diffTime

-- | Precision for @dayInScale@.
data DayPrecision = DayP
                  | MonthP

-- | Generates @Time.Day@ values with month precision, e.g. 2014-03-1.
dayInScale :: DayPrecision -> Gen Time.Day
dayInScale p =
  case p of
    DayP   -> Time.fromGregorian <$> choose (1858, 2050) <*> choose (1, 12) <*> choose (1, 31)
    MonthP -> Time.fromGregorian <$> choose (1858, 2050) <*> choose (1, 12) <*> pure 1

-- function to convert list of bytestring into hex digits - useful for debugging kx IPC bytes
bprint :: C.ByteString -> String
bprint x = ("0x" ++ ) $ foldl (++) "" $ printf "%02x" <$> C.unpack x

-----------------------------------------------------------------
-- $network
-- Network utils

-- | Finds a free port that can be bound to.
findFreePort :: IO PortNumber
findFreePort = go 1025 1100
  where go n stop | n <= stop = catchIOError (do
          sock <- socket AF_INET Stream defaultProtocol
          bindSocket sock (SockAddrInet n iNADDR_ANY)
          close sock
          return n)
          (\e -> print (show e) >> go (n+1) stop)
        go _ _              = ioError $ userError "No free ports"

--------------------------------------
-- $kdbutils
-- Kdb utils

user :: ByteString
user = "user"

pass :: ByteString
pass = "pwd"

-- | Starts a kdb instance on a free port with default @user@ and @pass@.
startKdb :: ResourceT IO PortNumber
startKdb = do
  freePort <- lift . liftIO $ findFreePort
  -- Find kdb binary and make it executable
  (qhome, kdb) <- lift . liftIO $ kdbExecutable
  lift . liftIO $ (do
    p <- getPermissions kdb
    setPermissions kdb (setOwnerExecutable True p))
  (_, h) <- allocate (openFile "/dev/null" AppendMode) hClose
  let cmd = (Process.proc kdb [ "-p", show freePort
                              , "-u", encodeString (fromString qhome </> fromString "users.txt")
                              ]) {
           Process.std_out = Process.UseHandle h
         , Process.std_err = Process.UseHandle h
         , Process.close_fds = True
         , Process.env     = Just [("QHOME", qhome)]
         }
      terminateProcess (_, _, _, ph) = void $ Process.terminateProcess ph >> Process.waitForProcess ph
  void $ allocate (Process.createProcess cmd) terminateProcess
  return $! freePort

-- | Gets the absolute paths to the QHOME and kdb executable.
kdbExecutable :: IO (String, String)
kdbExecutable = do
    basepath <- getCurrentDirectory
    let base = fromString basepath </> "tools" </> "kdb"
        kdb  = base </> kdbPath
    (return . (encodeString *** encodeString)) (base, kdb)
  where kdbPath = case os of
                 "darwin" -> "m32" </> "q"
                 "linux"  -> "l32" </> "q"
                 _        -> error "Unknown OS"

-- | Retries connecting to kdb until it succeeds,
-- sleeping for short periods of time in between retries.
--
-- The connection will be retried up to 100 times with a constant
-- 50ms delay or until a non @IOError@ is thrown.
kdbConnection :: ByteString -> ByteString -> PortNumber -> ResourceT IO Connection
kdbConnection userVal passVal freePort = do
  let -- 50 millis, max 100 times
      policy = constantDelay 50000 <> limitRetries 100
      props = def & port     .~ freePort
                  & username .~ Just userVal
                  & password .~ Just passVal
      -- Only handle IOExceptions until the process comes up
      -- and creates a socket: pass any other exceptions through
      h _ = Handler $ \ (_ :: IOError) -> return True

  -- Try connecting
  (_, con) <- allocate (recovering policy [h] $ Client.connect props) Client.close
  return $! con

--------------------------------------
-- *general
-- General utils

assertException :: (Exception e, Eq e, MonadIO m, MonadCatch m) => (e -> Bool) -> m a -> m ()
assertException ex action =
    E.catchIf ex (do
        _ <- action
        liftIO $ assertFailure $ "Unexpected exception")
        (const $ return ())
