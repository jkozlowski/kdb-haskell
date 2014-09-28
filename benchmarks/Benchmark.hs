{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Benchmark
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- All benchmarks.
-----------------------------------------------------------------------------
module Main where

import           Criterion.Main
import           Criterion.Measurement
import           Data.ByteString ()
import           Data.Fixed (Pico)
import           Data.Ratio
import           Data.Int (Int16)
import           Data.Monoid ((<>))
import           Database.Kdb.Internal.Types
import           Foreign.C.Types (CChar)
import           Text.Printf (printf)
import           Test.QuickCheck (Gen, arbitrary, listOf1, choose, generate, vectorOf, suchThat)
import qualified Control.Monad                as CM
import qualified Data.ByteString.Char8        as C
import qualified Data.Time                    as Time
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Database.Kdb.Internal.Types  as KT
import qualified Database.Kdb.Internal.IPC    as IPC

-- Unsafe stuff
import System.IO.Unsafe (unsafePerformIO)

-- Networking
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

-- function to convert list of bytestring into hex digits - useful for debugging kx IPC bytes
bprint :: C.ByteString -> String
bprint x = ("0x" ++ ) $ foldl (++) "" $ fmap (printf "%02x") $ C.unpack x

-- test:([]kbool:();kbyte:();kshort:();kint:();klong:();
-- kreal:();kfloat:();kchar:();ksymbol:();kboolv:();kbytev:();kshortv:();kintv:();
-- klongv:();krealv:();kfloatv:();kcharv:();ksymbolv:())

-- .u.upd:{[t;x] if not -16=type first first x;a:.z.n; x:$[0>type first x;a,x;(enlist(count first x)#a),x]]; t insert x}
-- write schema statement: simply send a char vector

main1 :: IO ()
main1 = do
--  let shortv = KShortV (SV.fromList[1..10]) -- short list - for benchmark testing
--      il1 = V . KIntV $ SV.enumFromN 1 5000000 -- list of int
--      il2 = V . KIntV $ SV.enumFromN 1 5000000 -- list of int
--      il3 = V . KIntV $ SV.enumFromN 1 5000000 -- list of int
--      l1 = KList (V.fromList [il1,il2,il3]) -- general list of int
--      sl1 = symV ["a", "b", "c"]
--      t = KT.table' (V.fromList [sl1,l1]) -- table t:([] a:til 5000000;b:til 5000000;c:til 5000000)
--      sl2 = s "t" -- atomic symbol `t
--      cl1 = charV "insert" -- string ".u.insert"
--      gl1 = list [cl1,sl2,t] -- general list (".u.insert";`t;t). t is the table from above
{--
  defaultMain [
        bench "ShortV" $ whnf SV.fromList ([1..10]::[Int16])
        ,bench "IPC.qIPC ShortV" $ whnf (IPC.qIPC 0) (HV (SV.fromList[1..10]))
        --,bench "foldl T" $ whnf (V.foldl' (\x y -> x + size y) 3) (V.fromList [sl1,l1])
        ,bench "fillT" $ whnf fillT (V.fromList [sl1,l1])
        ,bench "fillS" $ whnf fillS [[97,0],[98,0],[99,0]]
        ,bench "IPC.qIPC Taable" $ whnf (IPC.qIPC 0) t
        ,bench "IPC.qIPC General List" $ whnf (IPC.qIPC 0) gl1
    ]
--}
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7777")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock "user:pwd\1\0"
  msg <- recv sock 1024
  putStrLn "Received authentication"
  print $ bprint msg

  -- send the schema
  sendAll sock . IPC.asyncIPC $ charV schema

  -- Gen the rows
  rows <- setupEnv 100

  -- Send the rows
  CM.forM_ rows $ \x -> sendAll sock $ IPC.asyncIPC x

  -- Close the socket
  sClose sock

setupEnv :: Int -> IO [KT.Value]
setupEnv count = CM.replicateM count (generate randomRow)

benchmark :: IO ()
benchmark = defaultMain [
    env (setupEnv 1000000) $ \ ~rows ->
        bgroup "bla" [
            bench "length" $ nf (map IPC.asyncIPC) rows
        ]
   ]

--benchmark2 :: IO ()
--benchmark2 = do
--    (m, time) <- flip measure 1 $ env setupEnv $ \ ~rows ->
--        bgroup "bla" [
--            bench "length" $ nf (map IPC.asyncIPC) rows
--        ]
--    return ()

main2 :: IO ()
main2 = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7777")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)

  -- Login
  sendAll sock "user:pwd\1\0"
  msg <- recv sock 1024
  putStrLn "Received authentication"
  print $ bprint msg

  -- Send the schema
  sendAll sock . IPC.asyncIPC $ charV schema

  sClose sock

main :: IO ()
main = main1

