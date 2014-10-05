{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.C
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Binding to the Kdb+ C Library.
--
-----------------------------------------------------------------------------
module Database.Kdb.Internal.C where

import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

#define KXVER 3
#include "k.h"

{#fun khpu as connect
 {`String', fromIntegral `Int', `String'} -> `Int' fromIntegral#}

main :: IO ()
main = do
 con <- connect "localhost" 7777 ""
 return ()




