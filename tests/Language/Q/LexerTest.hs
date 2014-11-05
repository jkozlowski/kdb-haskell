-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Q.LexerTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Language.Q.Lexer'.
-----------------------------------------------------------------------------
module Language.Q.LexerTest (tests) where

import           Language.Q.Lexer (Token(..), alexScanTokens, AlexPosn(..))
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "Language.Q.Lexer" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "HUnit" [
    testCase "LCURLY"     $ alexScanTokens "{" @?= [LCURLY (AlexPn 0 1 1)]
  , testCase "RCURLY"     $ alexScanTokens "}" @?= [RCURLY (AlexPn 0 1 1)]
  ]

