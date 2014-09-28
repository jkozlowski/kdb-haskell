{-# LANGUAGE OverloadedStrings #-}
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
module Database.Kdb.Internal.IntegrationTest (tests, findFreePort, kdbExecutable) where

import           Control.Applicative             ((<$>))
import           Control.Arrow                   ((***))
import           Control.Monad                   (forM_)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Resource
import           Control.Retry                   (constantDelay, limitRetries,
                                                  recoverAll)
import qualified Data.Attoparsec.ByteString      as A
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as B8
import           Data.Monoid                     ((<>))
import           Data.String                     (fromString)
import qualified Database.Kdb.Internal.IPC       as IPC
import           Database.Kdb.Internal.TestUtils (bprint, randomRows, schema,
                                                  tableName)
import           Database.Kdb.Internal.Types
import qualified Database.Kdb.Internal.Types     as KT
import           Filesystem.Path.CurrentOS       (encodeString, (</>))
import           Network.Socket                  hiding (recv)
import           Network.Socket.ByteString       (recv, sendAll)
import           Prelude                         hiding (FilePath)
import           System.Directory                (getCurrentDirectory,
                                                  getPermissions,
                                                  setOwnerExecutable,
                                                  setPermissions)
import           System.Info                     (os)
import           System.IO.Error                 (catchIOError)
import qualified System.Process                  as Process
import           Test.Tasty
import           Test.Tasty.HUnit                (Assertion, assertFailure,
                                                  testCase, (@?=))

tests :: TestTree
tests = testGroup "Database.Kdb.Internal.IntegrationTest" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "HUnit" [
    testCase "Integration Test" integrationTest
  ]

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

-- | Finds a free port that kdb can bind to.
findFreePort :: IO PortNumber
findFreePort = go 1025 1100
  where go n stop | n <= stop = catchIOError (do
          sock <- socket AF_INET Stream defaultProtocol
          bindSocket sock (SockAddrInet n iNADDR_ANY)
          close sock
          return n)
          (\e -> print (show e) >> go (n+1) stop)
        go _ _              = ioError $ userError "No free ports"

-- | `count select from testTable` statement.
selectCount :: Value
selectCount = charV $ "count select from " <> tableName

-- | Retries connecting to kdb until it succeeds,
-- sleeping for short periods of time in between retries.
--
-- The connection will be retried up to 100 times with a constant
-- 50ms delay.
kdbConnection :: String -> ResourceT IO Socket
kdbConnection port = do
  addrinfos <- lift $ getAddrInfo Nothing (Just "127.0.0.1") (Just port)
  let serveraddr = head addrinfos
      -- 50 millis, max 100 times
      policy = constantDelay 50000 <> limitRetries 100

  -- Try connecting
  sock <- recoverAll policy $ do
    (_, sock) <- allocate (socket (addrFamily serveraddr) Stream defaultProtocol) sClose
    lift $ connect sock (addrAddress serveraddr)
    return sock

  lift $ sendAll sock "user:pwd\1\0"
  msg <- lift $ recv sock 1024
  lift $ putStrLn "Received authentication"
  lift $ print $ bprint msg

  return sock

-- | Starts up a kdb process and connects to it to
-- send a bunch of messages and select them to compare.
integrationTest :: Assertion
integrationTest = do
  let numRows = 100

  -- Find kdb binary and make it executable
  (qhome, kdb) <- kdbExecutable
  p <- getPermissions kdb
  setPermissions kdb (setOwnerExecutable True p)

  -- Find a free port for kdb to bind to
  port <- show <$> findFreePort

  -- Start kdb with this port and QHOME
  let cmd = (Process.proc kdb ["-p", port]) {
         Process.std_out = Process.Inherit
       , Process.env     = Just [("QHOME", qhome)]
       }
      terminateProcess (_, _, _, ph) = Process.terminateProcess ph

  runResourceT $ do

    -- Startup kdb
    _ <- allocate (Process.createProcess cmd) terminateProcess

    -- Connect to kdb
    sock <- kdbConnection port

    -- send the schema
    lift $ sendAll sock . IPC.asyncIPC $ charV schema

    -- Gen the rows
    rows <- lift $ randomRows numRows

    -- Send the rows
    lift $ forM_ rows $ \x -> sendAll sock $ IPC.asyncIPC x

    -- Select the rows
    lift $ sendAll sock . IPC.syncIPC $ selectCount

    -- Parse the rows
    value <- lift $ A.parseWith (recv sock 1024) IPC.ipcParser B.empty
    lift $ case value of
      (A.Fail left ctxs msg) -> assertFailure $ "Parse failed: " <> B8.unpack left <> " " <> show ctxs <> show msg
      (A.Partial _)          -> assertFailure "Ran out of input"
      (A.Done _ r)           -> r @?= KT.long numRows
