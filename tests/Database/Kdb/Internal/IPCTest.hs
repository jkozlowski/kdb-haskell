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

import           Control.Monad (forM_)
import           Data.ByteString.Char8 (ByteString, unpack)
import           Data.ByteString.Base16 (encode, decode)
import           Database.Kdb.Internal.Types (byV, i, iV, iVV, lI, sVV, dict)
import           Test.Tasty
--import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit (assertEqual, Assertion, testCase)
import qualified Database.Kdb.Internal.IPC as IPC

tests :: TestTree
tests = testGroup "Database.Kdb.Internal.IPC" [ qcProps, unitTests ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "simpleTests" simpleTests

  ]

-- | Tests taken from <http://code.kx.com/wiki/Reference/ipcprotocol>.
simpleTests :: Assertion
simpleTests = forM_ t $ \(msg, actual, expected) -> do
        let actualIPC = IPC.asyncIPC actual
        assertEqual (msg' msg actualIPC expected) actualIPC (fst . decode $ expected)
    where t = [
              ("q)-8!1", i 1, "010000000d000000fa01000000")
            , ("q)-8!enlist 1", iV [1], "010000001200000006000100000001000000")
            , ("q)-8!`byte$til 5", byV [0..4] , "01000000130000000400050000000001020304")
            , ("q)-8!`byte$enlist til 5", lI [byV [0..4]], "01000000190000000000010000000400050000000001020304")
            , ("q)-8!`a`b!2 3", dict (sVV ["a", "b"]) (iVV [2, 3]), "0100000021000000630b0002000000610062000600020000000200000003000000")
            ]
          msg' :: String -> ByteString -> ByteString -> String
          msg' msg actualIPC expected = msg ++ "\n   actual=" ++ (unpack . encode $ actualIPC) ++ "\n expected=" ++ (unpack expected)
