{
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Q.Parser
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Parser for the q language.
-----------------------------------------------------------------------------
module Language.Q.Parser
  ( parse
  ) where

import Language.Q.Lexer
import Language.Q.Types
}

%name parse
%tokentype { Token }
%monad { Alex } { >>= } { return }
%lexer { lexwrap } { EOF }
-- Without this we get a type error
%error { happyError }

%token
       s           { STRING $$ }

%%

SimpleDT : s            { StringDT $1 }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap cont = do
  t <- alexMonadScan'
  cont t

-- We rewrite alexMonadScan' to return the position when lexing fails (the
-- default implementation just returns an error message).
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (pos, _, _, _) -> alexError (show pos)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

getPosn :: Alex (Int,Int)
getPosn = do
  (AlexPn _ l c,_,_,_) <- alexGetInput
  return (l,c)

happyError :: Token -> Alex a
happyError t = do
  (l,c) <- getPosn
  fail (show l ++ ":" ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n")
}
