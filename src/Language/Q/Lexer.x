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
  ( alexSetInput
  , alexGetInput
  , alexError
  , alexScan
  , ignorePendingBytes
  , alexGetStartCode
  , runAlex
  , Alex(..)
  , AlexReturn(..)
  , AlexPosn(..)
  , eof
  , alexEOF
    -- Tokens
  , Lexeme(..)
  , Token(..)
    -- Simple scanner for testing.
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
  \` $sym*      { \(p,_,_,s) i -> return . Lexeme p . SYM $ take (i - 1) . tail $ s }
  \" @string* \" { \(p,_,_,s) i -> return . Lexeme p . STRING $ take (i - 2) . tail $ s }

{
-- | Main token type that indicates the position of the token in the source file.
data Lexeme = Lexeme {-# UNPACK #-} !AlexPosn {-# UNPACK #-} !Token
  deriving (Eq, Show)

-- | Possible tokens in a q script.
data Token
  = LCURLY
  | RCURLY
  | COLON
  | COLONCOLON
  | SEMICOLON
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | COMMA
  | POINT
  | PLUS
  | MINUS
  | MULT
  | DIV
  | APPLY
  | QUESTION
  | DOLLAR
  | NOT_EQUAL
  | EQUAL
  | LESS_THAN
  | GREATER_THAN
  | EXCL
  | DROP_CUT
  | FILL
  | TAKE
  | MATCH
  | SYM           {-# UNPACK #-} String
  | STRING        {-# UNPACK #-} String
  | EOF
  deriving (Eq, Show)

-- | Returns the @EOF@ token in the monad.
alexEOF = return $! Lexeme (AlexPn 0 0 0) EOF
{-# INLINE alexEOF #-}

-- | @EOF@ lexeme: the position is set to nonsense.
eof = Lexeme (AlexPn 0 0 0) EOF

-- For constructing tokens that only capture the position.
lex' :: Token -> AlexAction Lexeme
lex' t = \(p,_,_,_) _ -> return $ Lexeme p t

-- | Lexes a string to a list of tokens.
scanner :: String -> Either String [Lexeme]
scanner str = runAlex str $ do
  let loop :: Alex [Lexeme]
      loop = do l <- alexMonadScan
                if l == eof
                  then return [l]
                  else do ls <- loop
                          return $! l : ls
  loop
}