{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.Client
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Contains functions for writing to and querying Kdb+
-- and connection management.
--
-- All functions in this modules rethrow any exceptions after cleanly
-- closing the underlying socket.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.Client (
    -- $connection
    -- Connection management
    connect, close

    -- $actions
    -- Reading and writing.
  , writeKdb, query
  )
  where

import           Blaze.ByteString.Builder                 (Builder)
import qualified Blaze.ByteString.Builder                 as Blaze
import qualified Blaze.ByteString.Builder.Internal.Buffer as Blaze
import           Control.Lens
import           Control.Monad.Catch                      (MonadCatch,
                                                           MonadMask)
import qualified Control.Monad.Catch                      as E
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8                    as B8
import qualified Database.Kdb.Internal.IPC                as IPC
import           Database.Kdb.Internal.Types.ClientTypes  (Connection (..), ConnectionSettings (..), InvalidCredentials (..),
                                                           inputStream, loginBytesFromConnectionSettings,
                                                           outputStream)
import qualified Database.Kdb.Internal.Types.KdbTypes     as Kdb
import qualified Network.Socket                           as NS
import qualified System.IO.Streams                        as Streams
import qualified System.IO.Streams.Attoparsec             as Streams

-----------------------------------------------------------------------------
-- $connection
-- Connection management functions.

-- | Creates a connection using the specified @ConnectionSettings@.
--
-- This function will rethrow any exceptions coming from @Network.Socket@
-- after releasing any aquired resources.
--
-- This function will throw @InvalidCredentials@ exception if Kdb rejects
-- the login credentials.
--
-- This function will throw @IOError@ in case of unexpected reply from Kdb.
connect :: MonadIO m
        -- ^ Setting to use
        => ConnectionSettings
        -- ^ Opaque connection to Kdb+.
        -> m Connection
connect cs@ConnectionSettings {..} =

  liftIO $ E.bracketOnError mkSocket NS.close open

  where
    -- Attempts to create the socket.
    mkSocket = correctConnect _host (show _port)

    -- Establishes the connection
    open socket = do
      (is, osNaked)
          <- Streams.socketToStreamsWithBufferSize _receiveBufferSize
                                                   socket
      os <- Streams.unsafeBuilderStream (Blaze.allocBuffer _writeBufferSize) osNaked

      -- Write the login bytes
      writeAndFlush (loginBytesFromConnectionSettings cs) os

      -- Read and parse the capability
      capabilityBytes <- readCapability is
      let maybeVersion = IPC.capabilityParser capabilityBytes

      case maybeVersion of
        Just _  -> return $! Connection {
                        _settings     = cs
                      , _socket       = socket
                      , _inputStream  = is
                      , _outputStream = os
                      }
        Nothing -> invalidCapability capabilityBytes

    -- | Reads the capability from the 'is' or throws @InvalidCredentials@.
    readCapability is
      = Streams.readExactly 1 is
        `E.catch`
        \(_ :: Streams.ReadTooShortException) -> E.throwM InvalidCredentials

    -- | Throws the invalid capability @IOError@.
    invalidCapability bs = E.throwM . userError $ "Server sent invalid capability byte: " ++ B8.unpack bs


-- | Closes the connection.
close :: MonadIO m
      => Connection
      -> m ()
close Connection {..} = liftIO $! NS.close _socket

-----------------------------------------------------------------------------
-- $actions
-- Primitive read/write actions.

-- | Asynchronous write to kdb.
writeKdb :: (MonadIO m, MonadCatch m)
         => Kdb.Value
         -> Connection
         -> m ()
writeKdb v = withConnection (liftIO . write)
  where write c = writeAndFlush (IPC.asyncIPC v) (c ^. outputStream)

-- | Writes the value to Kdb and expects a synchronous reply.
--
-- | Throws @Streams.ParseException@ on failure to parse the reply.
query :: (MonadIO m, MonadCatch m)
         => Kdb.Value
         -> Connection
         -> m Kdb.Value
query v = withConnection $ \c -> liftIO $ do
  let is = c ^. inputStream
      os = c ^. outputStream
  writeAndFlush (IPC.syncIPCB v) os
  Streams.parseFromStream IPC.ipcParser is

-- | Runs @action@ or closes the connection and rethrows any exceptions.
withConnection :: (MonadIO m, MonadCatch m)
               => (Connection -> m a)
               -> Connection
               -> m a
withConnection action c = E.onException (action c) (close c)

-----------------------------------------------------------------------------
-- Private functions.

-- | Tries to go through all addresses in @NS.getAddrInfo@ until one
-- of the connections succeedes or rethrows the last exception.
--
-- Code mostly copied from network-simple except that it iterates
-- through the results, trying to find a working address.
correctConnect :: (MonadIO m, MonadCatch m, MonadMask m)
               => NS.HostName
               -> NS.ServiceName
               -> m NS.Socket
correctConnect host port = do
  addrs <- addrInfos
  let exceptions = map tryConnect addrs
  firstOrException exception exceptions
  where
    hints :: NS.AddrInfo
    hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG]
                            , NS.addrSocketType = NS.Stream
                            }
    -- The addrInfos is guaranteed to be non-empty
    exception = userError "Not going to happen!"
    addrInfos = liftIO $ NS.getAddrInfo (Just hints) (Just host) (Just port)

-- | Tries to create a socket and returns the socket if successfull
-- or the exception if not successful.
tryConnect :: (MonadIO m, MonadMask m, MonadCatch m, E.Exception e)
           => NS.AddrInfo
           -> m (Either e NS.Socket)
tryConnect addr = E.try $ E.bracketOnError newSocket closeSocket connectSocket
  where newSocket = liftIO $  NS.socket (NS.addrFamily addr)
                                        (NS.addrSocketType addr)
                                        (NS.addrProtocol addr)
        connectSocket sock = liftIO $ do
          let sockAddr = NS.addrAddress addr
          NS.connect sock sockAddr
          return $! sock
        closeSocket = liftIO . NS.close

-- | Returns the first @Right@ value or @e@ if empty,
-- or last @Left@ value if non-empty.
firstOrException :: (MonadCatch m, MonadMask m, E.Exception e)
                 => e
                 ->[m (Either e a)]
                 -> m a
firstOrException ex []     = E.throwM ex
firstOrException _  (ea:eas) = ea >>= \e -> case e of
     Right s  -> return $! s
     Left  ex -> firstOrException ex eas

writeAndFlush :: Builder -> Streams.OutputStream Builder -> IO ()
writeAndFlush b os = do
  Streams.write (Just b)           os
  Streams.write (Just Blaze.flush) os
