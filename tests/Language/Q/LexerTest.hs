{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- ,
-- Module      :  Language.Q.LexerTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for @Language.Q.Lexer@.
-----------------------------------------------------------------------------
module Language.Q.LexerTest (tests) where

import           Language.Q.Lexer      (AlexPosn (..), Lexeme (..), Token (..),
                                        eof, scanner)
import           Test.QuickCheck
import           Test.QuickCheck.IO    (propertyIO)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertEqual, testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Language.Q.Lexer" [ unitTests, props ]

unitTests :: TestTree
unitTests = testGroup "HUnit"
    (map (\(s, t) ->
      let expected = [Lexeme (AlexPn 0 1 1) t, eof]
      in testCase (show expected) $ testLex s @?= expected) tokens)

props :: TestTree
props = testGroup "QuickCheck" [
    testProperty "Generated correct files" $
      forAll (listOf . elements $ tokens) correct
  ]

-- Hack, hack, hack
instance Show (AlexPosn -> Token) where
    show _ = "Token"

-- | Tests that the lexer can lex space-separated strings of tokens.
correct :: [(String, Token)] -> Gen Prop
correct tkns = propertyIO $
  let tokens' _ []           = [eof]
      tokens' c ((s', t):xs) = Lexeme (AlexPn c 1 (c + 1)) t : tokens' (c + length s' + 1) xs
      s = unwords $ map fst tkns
      msg = "text=" ++ s
  in assertEqual msg (tokens' 0 tkns) (testLex s)

tokens :: [(String, Token)]
tokens = [
    ("{"  , LCURLY       )
  , ("}"  , RCURLY       )
  , (":"  , COLON        )
  , ("::" , COLONCOLON   )
  , (";"  , SEMICOLON    )
  , ("["  , LBRACK       )
  , ("]"  , RBRACK       )
  , ("("  , LBRACE       )
  , (")"  , RBRACE       )
  , (","  , COMMA        )
  , ("."  , POINT        )
  , ("+"  , PLUS         )
  , ("-"  , MINUS        )
  , ("*"  , MULT         )
  , ("%"  , DIV          )
  , ("@"  , APPLY        )
  , ("?"  , QUESTION     )
  , ("$"  , DOLLAR       )
  , ("<>" , NOT_EQUAL    )
  , ("="  , EQUAL        )
  , ("<"  , LESS_THAN    )
  , (">"  , GREATER_THAN )
  , ("!"  , EXCL         )
  , ("_"  , DROP_CUT     )
  , ("^"  , FILL         )
  , ("#"  , TAKE         )
  , ("~"  , MATCH        )
  , ("`hello",    SYM "hello")
  , ("\"Hello\"", STRING "Hello")
  , ("H"  , ID "H")
  , ("H_1", ID "H_1")
  ]

testLex :: String -> [Lexeme]
testLex s = case scanner s of
          Left msg -> error msg
          Right a  -> a
