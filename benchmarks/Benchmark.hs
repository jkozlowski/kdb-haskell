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

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain [
      bench "fib 10" $ nf (\n -> fib (10+n-n)) 10
    , bench "fib 30" $ nf (\n -> fib (30+n-n)) 30
    , bench "fib 35" $ nf (\n -> fib (35+n-n)) 35
    ]
