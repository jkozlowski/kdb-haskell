{-# LANGUAGE OverloadedStrings #-}
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

import qualified Blaze.ByteString.Builder             as Blaze
import           Control.Applicative                  ((<$>))
import qualified Control.Monad                        as CM
import           Criterion.Main
import           Data.ByteString                      ()
import qualified Data.ByteString.Char8                as C
import qualified Database.Kdb.Internal.IPC            as IPC
import           Database.Kdb.Internal.TestUtils      (randomRow)
import qualified Database.Kdb.Internal.Types.KdbTypes as KT
import           Test.QuickCheck                      (generate)
import           Text.Printf                          (printf)

-- function to convert list of bytestring into hex digits - useful for debugging kx IPC bytes
bprint :: C.ByteString -> String
bprint x = ("0x" ++ ) $ foldl (++) "" $ printf "%02x" <$> C.unpack x

setupEnv :: Int -> IO [KT.Value]
setupEnv count = CM.replicateM count (generate randomRow)

benchmark :: IO ()
benchmark = defaultMain [
    env (setupEnv 1000000) $ \ ~rows ->
        bgroup "IPC" [
            bench "asyncIPC" $ nf (map (Blaze.toByteString . IPC.asyncIPC)) rows
        ]
   ]

main :: IO ()
main = benchmark

