{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.Types.ClientTypes
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Defines types used by @Database.Kdb.Internal.Client@ code.
--
-- All the constructors are exported for use in internal code, however
-- those should not be exported to client code later.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.Types.ClientTypes (
    -- * Settings for the connection and lenses
    -- $connectionsettings
    ConnectionSettings(..)
  , host, port, username, password, writeBufferSize, receiveBufferSize, version

    -- * The main connection type
    -- $connection
  , Connection(..), settings, socket, inputStream, outputStream
  , loginBytesFromConnectionSettings

    -- * Exception types
    -- $exceptions
  , InvalidCredentials(..)
  ) where

import           Blaze.ByteString.Builder  (Builder)
import           Control.Lens
import           Control.Monad.Catch       (Exception)
import           Data.ByteString           (ByteString)
import           Data.Default.Class        (Default, def)
import           Data.Typeable             (Typeable)
import qualified Database.Kdb.Internal.IPC as IPC
import           Network.Socket            (PortNumber, Socket)
import qualified System.IO.Streams         as Streams

-----------------------------------------------------------------------------
-- $connectionsettings
--
-- Settings necessary in order to establish a connection.

-- | Connection settings.
--
-- In order to set your params use the lenses provided, for example:
--
-- @
--   import Control.Lens
--   import Data.Default.Class
--
--   myConfig = def & host .~ "somehostname"
--                  & port .~ 5050
--                  & receiveBufferSize  .~ 1234
-- @
data ConnectionSettings = ConnectionSettings
    { -- ^ Hostname (kept as a string for simplicity)
    _host                :: !String

    -- ^ Port Number
    , _port              :: !PortNumber

    -- ^ Username (possibly empty)
    , _username          :: !(Maybe ByteString)

    -- ^ Password (possibly empty)
    , _password          :: !(Maybe ByteString)

    -- ^ Size of the write buffer (in bytes)
    , _writeBufferSize   :: !Int

    -- ^ Size of the receive buffer (in bytes)
    , _receiveBufferSize :: !Int

    -- ^ Version of the protocol
    , _version           :: !IPC.Version
    } deriving (Show, Eq)

-- | Default @ConnectionSettings@.
--
-- * host = "localhost"
-- * port = 5010
-- * username = Nothing
-- * password = Nothing
-- * writeBufferSize = 4096
-- * receiveBufferSize = 4096
-- * version = V_30
instance Default ConnectionSettings where
  def = ConnectionSettings {
          -- https://www.haskell.org/pipermail/haskell-cafe/2010-June/078602.html
          _host = "localhost"
        , _port = 5010
        , _username = Nothing
        , _password = Nothing
        , _writeBufferSize = 4096
        , _receiveBufferSize = 4096
        , _version = IPC.V_30
        }

-----------------------------------------------------------------------------
-- $connection
--
-- The main connection type.

-- | State of a connection to Kdb+.
data Connection = Connection
    { _settings     :: !ConnectionSettings
    , _socket       :: !Socket
    , _inputStream  :: !(Streams.InputStream ByteString)
    , _outputStream :: !(Streams.OutputStream Builder)
    }

makeLenses ''ConnectionSettings
makeLenses ''Connection

-- | Creates a login message for these @ConnectionSettings@.
loginBytesFromConnectionSettings :: ConnectionSettings -> Builder
loginBytesFromConnectionSettings ConnectionSettings {..} = IPC.loginBytes _username _password _version
{-# INLINE loginBytesFromConnectionSettings #-}

-----------------------------------------------------------------------------
-- $exceptions
--
-- The possible exceptions.

-- | Thrown by the 'connect' function when Kdb+ rejects the credentials.
data InvalidCredentials = InvalidCredentials
 deriving (Eq, Typeable)

instance Show InvalidCredentials where
    show InvalidCredentials = "Invalid credentials"

instance Exception InvalidCredentials
