{
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
module Language.Q.Lexer (
    Token(..)
  , AlexPosn(..)
  , alexScanTokens
  ) where
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  "{"         { sTok LCURLY        }
  "}"         { sTok RCURLY        }
  ":"         { sTok COLON         }
  ";"         { sTok SEMICOLON     }
  "["         { sTok LBRACK        }
  "]"         { sTok RBRACK        }
  "("         { sTok LBRACE        }
  ")"         { sTok RBRACE        }
  "\""        { sTok DQUOTE        }
  ","         { sTok COMMA         }
  "."         { sTok POINT         }
  "+"         { sTok PLUS          }
  "-"         { sTok MINUS         }
  "*"         { sTok MULT          }
  "%"         { sTok DIV           }
  "@"         { sTok APPLY         }
  "?"         { sTok QUESTION      }
  "$"         { sTok DOLLAR        }
  "<>"        { sTok NOT_EQUAL     }
  "="         { sTok EQUAL         }
  "<"         { sTok LESS_THAN     }
  ">"         { sTok GREATER_THAN  }
  "!"         { sTok EXCL          }
  "_"         { sTok DROP_CUT      }
  "^"         { sTok FILL          }
  "#"         { sTok TAKE          }
  "~"         { sTok MATCH         }

{

-- Some action helpers:
tok f p s = f p s

sTok f = \p s -> f p

-- | Possible tokens in a q script.
data Token
  = LCURLY       AlexPosn
  | RCURLY       AlexPosn
  | COLON        AlexPosn
  | SEMICOLON    AlexPosn
  | LBRACK       AlexPosn
  | RBRACK       AlexPosn
  | LBRACE       AlexPosn
  | RBRACE       AlexPosn
  | DQUOTE       AlexPosn
  | COMMA        AlexPosn
  | POINT        AlexPosn
  | PLUS         AlexPosn
  | MINUS        AlexPosn
  | MULT         AlexPosn
  | DIV          AlexPosn
  | APPLY        AlexPosn
  | QUESTION     AlexPosn
  | DOLLAR       AlexPosn
  | NOT_EQUAL    AlexPosn
  | EQUAL        AlexPosn
  | LESS_THAN    AlexPosn
  | GREATER_THAN AlexPosn
  | EXCL         AlexPosn
  | DROP_CUT     AlexPosn
  | FILL         AlexPosn
  | TAKE         AlexPosn
  | MATCH        AlexPosn
  deriving (Eq, Show)
}