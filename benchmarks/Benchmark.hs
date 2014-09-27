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
import           Data.Int (Int16)
import           Database.Kdb.Internal.Types (Value(..), cV, s, li)
import           Foreign.C.Types (CChar)
import           Text.Printf (printf)
import qualified Control.Monad                as CM (forM_)
import qualified Data.ByteString.Char8        as C
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

fillS :: [[CChar]] -> Value
fillS x = let (x',y') = createS x
          in SV x' y'
{-# INLINE fillS #-}

createS :: [[CChar]] -> (Int, SV.Vector CChar)
createS cl = unsafePerformIO $ do
            v <- MSV.new (Prelude.length . Prelude.concat $ cl)
            fill v 0 $ Prelude.concat cl
            SV.unsafeFreeze v >>= \x -> return (Prelude.length cl,x)
          where
            fill _ _ [] = return ()
            fill v i (x:xs) = MSV.unsafeWrite v i x >> fill v (i + 1) xs

-- | Constructor for T - a Q table - we must always build it using this function
-- 2 bytes for table header - 1 additional byte for dict type header
fillT :: V.Vector Value -> Value
fillT !xs = T (V.foldl' (\x y -> x + KT.size y) 3 xs) xs
{-# INLINE fillT #-}

-- function to convert list of bytestring into hex digits - useful for debugging kx IPC bytes
bprint :: C.ByteString -> String
bprint x = ("0x" ++ ) $ foldl (++) "" $ fmap (printf "%02x") $ C.unpack x

main :: IO ()
main = do
  let shortv = HV (SV.fromList[1..10]) -- short list - for benchmark testing
      il1 = IV $ SV.enumFromN 1 5000000 -- list of int
      il2 = IV $ SV.enumFromN 1 5000000 -- list of int
      il3 = IV $ SV.enumFromN 1 5000000 -- list of int
      l1 = L (V.fromList [il1,il2,il3]) -- general list of int
      sl1 = fillS [[97,0],[98,0],[99,0]] -- symbol list: `a`b`c
      t = fillT (V.fromList [sl1,l1]) -- table t:([] a:til 5000000;b:til 5000000;c:til 5000000)
      sl2 = s "t" -- atomic symbol `t
      cl1 = cV "insert" -- string ".u.insert"
      gl1 = li [cl1,sl2,t] -- general list (".u.insert";`t;t). t is the table from above
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
  print "Received authentication"
  print $ bprint msg
  CM.forM_ [1..1] $ \x -> sendAll sock $ IPC.asyncIPC gl1
  sClose sock

