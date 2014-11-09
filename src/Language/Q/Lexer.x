{
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Q.Lexer
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Lexer for the q language.
-----------------------------------------------------------------------------
module Language.Q.Lexer
  ( skip
  , andBegin
  , alexEOF
  , alexSetInput
  , alexGetInput
  , alexError
  , alexScan
  , alexMonadScan
  , ignorePendingBytes
  , alexGetStartCode
  , runAlex
  , Alex(..)
  , Token(..)
  , AlexReturn(..)
  , AlexPosn(..)
  , scanner
  ) where

import Prelude hiding (lex)
import Control.Applicative
}

%wrapper "monad"

$digit  = 0-9            -- digits
$letter = [a-zA-Z]       -- alphabetic characters
@colon  = ":"
$sym    = [$letter $digit \_ \. \/ \:]

-- String handling with escape sequences
$escchars = [abfnrtv\\"\'&]
@escape = \\ ($escchars | \0)
@gap = \\ $white+ \\
@string = $printable # [\"] | " " | @escape
@nl     = "\n"

-- Number types
@longtype   = "j"
@shorttype  = "h"
@realtype   = "e"

-- Temporal types
@minutetype = "u"
@secondtype = "v"
@timetype   = "t"

-- Date types
@monthtype    = "m"
@datetype     = "d"
@datetimetype = "z"
@temporaltype = @minutetype
              | @secondtype
              | @timetype


-- Nulls
@nullextension = @longtype
               | @shorttype
               | @realtype
               | @temporaltype
               | @monthtype
               | @datetype
               | @datetimetype


tokens :-
  -- Simple tokens
  $white+     ;
  "{"           { lex' LCURLY        }
  "}"           { lex' RCURLY        }
  @colon        { lex' COLON         }
  @colon @colon { lex' COLONCOLON    }
  ";"           { lex' SEMICOLON     }
  "["           { lex' LBRACK        }
  "]"           { lex' RBRACK        }
  "("           { lex' LBRACE        }
  ")"           { lex' RBRACE        }
  ","           { lex' COMMA         }
  "."           { lex' POINT         }
  "+"           { lex' PLUS          }
  "-"           { lex' MINUS         }
  "*"           { lex' MULT          }
  "%"           { lex' DIV           }
  "@"           { lex' APPLY         }
  "?"           { lex' QUESTION      }
  "$"           { lex' DOLLAR        }
  "<>"          { lex' NOT_EQUAL     }
  "="           { lex' EQUAL         }
  "<"           { lex' LESS_THAN     }
  ">"           { lex' GREATER_THAN  }
  "!"           { lex' EXCL          }
  "_"           { lex' DROP_CUT      }
  "^"           { lex' FILL          }
  "#"           { lex' TAKE          }
  "~"           { lex' MATCH         }
  -- More complex tokens
  -- TODO: Need to prohibit multiline strings
  \` $sym*      { \(p,_,_,s) i -> return . SYM p $ take (i - 1) . tail $ s }
  \" @string* \" { \(p,_,_,s) i -> return . STRING p $ take (i - 2) . tail $ s }

{
-- | Possible tokens in a q script.
data Token
  = LCURLY        {-# UNPACK #-} !AlexPosn
  | RCURLY        {-# UNPACK #-} !AlexPosn
  | COLON         {-# UNPACK #-} !AlexPosn
  | COLONCOLON    {-# UNPACK #-} !AlexPosn
  | SEMICOLON     {-# UNPACK #-} !AlexPosn
  | LBRACK        {-# UNPACK #-} !AlexPosn
  | RBRACK        {-# UNPACK #-} !AlexPosn
  | LBRACE        {-# UNPACK #-} !AlexPosn
  | RBRACE        {-# UNPACK #-} !AlexPosn
  | COMMA         {-# UNPACK #-} !AlexPosn
  | POINT         {-# UNPACK #-} !AlexPosn
  | PLUS          {-# UNPACK #-} !AlexPosn
  | MINUS         {-# UNPACK #-} !AlexPosn
  | MULT          {-# UNPACK #-} !AlexPosn
  | DIV           {-# UNPACK #-} !AlexPosn
  | APPLY         {-# UNPACK #-} !AlexPosn
  | QUESTION      {-# UNPACK #-} !AlexPosn
  | DOLLAR        {-# UNPACK #-} !AlexPosn
  | NOT_EQUAL     {-# UNPACK #-} !AlexPosn
  | EQUAL         {-# UNPACK #-} !AlexPosn
  | LESS_THAN     {-# UNPACK #-} !AlexPosn
  | GREATER_THAN  {-# UNPACK #-} !AlexPosn
  | EXCL          {-# UNPACK #-} !AlexPosn
  | DROP_CUT      {-# UNPACK #-} !AlexPosn
  | FILL          {-# UNPACK #-} !AlexPosn
  | TAKE          {-# UNPACK #-} !AlexPosn
  | MATCH         {-# UNPACK #-} !AlexPosn
  | SYM           {-# UNPACK #-} !AlexPosn {-# UNPACK #-} String
  | STRING        {-# UNPACK #-} !AlexPosn {-# UNPACK #-} String
  | EOF
  deriving (Eq, Show)

-- | Returns the @EOF@ token in the monad.
alexEOF = return EOF

-- For constructing tokens that only capture the position.
lex' :: (AlexPosn -> a) -> AlexAction a
lex' f = \(p,_,_,_) i -> return $ f p

-- | Lexes a string to a list of tokens.
scanner str = runAlex str $ do
  let loop :: Alex [Token]
      loop = do tok <- alexMonadScan
                if tok == EOF
                  then return [tok]
                  else do toks <- loop
                          return $! tok : toks
  loop
}