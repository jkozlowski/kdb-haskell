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

-- Simple tokens
@colon   = ":"
@dropcut = "_"
@point   = "."

-- General patterns
$digit  = 0-9            -- digits
$letter = [a-zA-Z]       -- alphabetic characters
$sym    = [$letter $digit \_ \. \/ \:]
@hex = $digit | [a-FA-F]

-- String handling with escape sequences
$escchars = [abfnrtv\\"\'&]
@escape = \\ ($escchars | \0)
@gap = \\ $white+ \\
@string = $printable # [\"] | " " | @escape
@nl     = "\n"

-- Complex named patterns
@id = $letter
    | $letter (@dropcut | $letter | $digit)* ($letter | $digit)
-- I cannot represent this in the lexer + I'm not sure it should be? | (@point @id)+

-- Number types
@longtype   = "j"
@shorttype  = "h"
@realtype   = "e"
@floattype  = "f"

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

@null = '0' ('x' '0' '0' | 'b' | 'N' @nullextension )

-- Infinities
@infinityextension = @longtype
                   | @shorttype
                   | @realtype
                   | @timetype
                   | @datetype
                   | @datetimetype

@infinity = '-'? '0' ('W' @infinityextension)

-- Primitives
@boolean        = '1' 'b'
@byte           = '0' 'x' @hex @hex?
@integer_prefix = ('-')? $digit+
@short          = @integer_prefix @shorttype
@long           = @integer_prefix @longtype
@float_prefix   = @integer_prefix '.' $digit*
@real           = @float_prefix @realtype
                | @integer_prefix @realtype
@float          = @float_prefix @floattype?
                | @integer_prefix @floattype
                | '0n'
                | '0w'
                | '-0w'
                | '0Nf'
                | '0Wf'
                | '-0Wf'
@integer        = @integer_prefix
                | '0N'
                | '0W'
                | '-0W'

q :-
  -- Simple tokens
<0>  $white+       ;
<0>  "{"           { lex' LCURLY        }
<0>  "}"           { lex' RCURLY        }
<0>  @colon        { lex' COLON         }
<0>  @colon @colon { lex' COLONCOLON    }
<0>  ";"           { lex' SEMICOLON     }
<0>  "["           { lex' LBRACK        }
<0>  "]"           { lex' RBRACK        }
<0>  "("           { lex' LBRACE        }
<0>  ")"           { lex' RBRACE        }
<0>  ","           { lex' COMMA         }
<0>  @point        { lex' POINT         }
<0>  "+"           { lex' PLUS          }
<0>  "-"           { lex' MINUS         }
<0>  "*"           { lex' MULT          }
<0>  "%"           { lex' DIV           }
<0>  "@"           { lex' APPLY         }
<0>  "?"           { lex' QUESTION      }
<0>  "$"           { lex' DOLLAR        }
<0>  "<>"          { lex' NOT_EQUAL     }
<0>  "="           { lex' EQUAL         }
<0>  "<"           { lex' LESS_THAN     }
<0>  ">"           { lex' GREATER_THAN  }
<0>  "!"           { lex' EXCL          }
<0>  @dropcut      { lex' DROP_CUT      }
<0>  "^"           { lex' FILL          }
<0>  "#"           { lex' TAKE          }
<0>  "~"           { lex' MATCH         }
  -- More complex tokens
  -- TODO: Need to prohibit multiline strings
--<0>  \` $sym*       { \(p,_,_,s) i -> return . Lexeme p . SYM $ take (i - 1) . tail $ s }
--<0>  \" @string* \" { \(p,_,_,s) i -> return . Lexeme p . STRING $ take (i - 2) . tail $ s }
<0>  \` $sym*       { \(p,_,_,s) i -> return . SYM    $! take (i - 1) . tail $ s }
<0>  \" @string* \" { \(p,_,_,s) i -> return . STRING $! take (i - 2) . tail $ s }
<0>  @id            { lex ID }

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
  | SYM           {-# UNPACK #-} !String
  | STRING        {-# UNPACK #-} !String
  | ID            {-# UNPACK #-} !String
  | EOF
  deriving (Eq, Show)

-- | Returns the @EOF@ token in the monad.
--alexEOF = return $! Lexeme (AlexPn 0 0 0) EOF
alexEOF = return $! EOF
--
{-# INLINE alexEOF #-}

-- | @EOF@ lexeme: the position is set to nonsense.
--eof = Lexeme (AlexPn 0 0 0) EOF
eof = EOF

-- | Creates a lexeme that takes a string parameter.
--lex :: (String -> Token) -> AlexAction Lexeme
--lex t = \(p,_,_,s) i -> return . Lexeme p . t $! take i s
lex :: (String -> Token) -> AlexAction Token
lex t = \(p,_,_,s) i -> return . t $! take i s

-- For constructing tokens that only capture the position.
--lex' :: Token -> AlexAction Lexeme
--lex' t = \(p,_,_,_) _ -> return $! Lexeme p t
lex' :: Token -> AlexAction Token
lex' t = \(_,_,_,_) _ -> return $! t

-- | Lexes a string to a list of tokens.
--scanner :: String -> Either String [Lexeme]
scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
--let loop :: Alex [Lexeme]
  let loop :: Alex [Token]
      loop = do l <- alexMonadScan
                if l == eof
                  then return [l]
                  else do ls <- loop
                          return $! l : ls
  loop
}