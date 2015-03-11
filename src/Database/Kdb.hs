-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Low-level Haskell bindings for KDB+.
--
--
-----------------------------------------------------------------------------
module Database.Kdb (
    -- * How to use this library
    -- $use

    module ClientTypes
  , module DateTimeTypes
  , module KdbTypes

  , module Client
  , module IPC
)
where

import           Database.Kdb.Internal.Types.ClientTypes   as ClientTypes
import           Database.Kdb.Internal.Types.DateTimeTypes as DateTimeTypes
import           Database.Kdb.Internal.Types.KdbTypes      as KdbTypes

import           Database.Kdb.Internal.Client              as Client
import           Database.Kdb.Internal.IPC                 as IPC

-- $use
--
-- This section contains basic information on how to read and write values
-- to kdb.
--
--
