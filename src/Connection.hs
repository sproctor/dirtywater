{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Connection where

import Control.Concurrent.STM
import Control.Exception
import Data.String.Class
import Data.Typeable
import qualified Data.ByteString as B
import Data.Conduit.Network.Server

import Types

data ExitException = ExitException deriving (Show, Typeable)

instance Exception ExitException

isExitException :: ExitException -> Bool
isExitException e =
  typeOf e == typeOf ExitException

getCommand :: PlayerConnection -> STM (Maybe Command)
getCommand pconn = tryReadTBQueue (connectionQueue pconn)

isEmptyCommandQueue :: PlayerConnection -> STM Bool
isEmptyCommandQueue conn =
  isEmptyTBQueue $ connectionQueue conn

--cPutStr :: (ConvString s, StringRWIO s) => PlayerConnection -> s -> IO ()
cPutStr :: PlayerConnection -> B.ByteString -> IO ()
cPutStr pconn str =
  sendToClient (connectionConn pconn) (fromString (toString str))

--cPutStrLn :: (ConvString s, StringRWIO s) => PlayerConnection -> s -> IO ()
cPutStrLn :: PlayerConnection -> B.ByteString -> IO ()
cPutStrLn pconn str = do
  cPutStr pconn str
  sendToClient (connectionConn pconn) "\n"

cGetLine :: PlayerConnection -> IO B.ByteString
cGetLine pconn = do
  line <- getFromClient (connectionConn pconn)
  case line of
    Nothing -> error "Disconnected client"
    Just l -> return l

newConnection :: GameState -> Conn -> Character -> STM PlayerConnection
newConnection gs conn char = do
  queue <- newTBQueue 10
  closed <- newTVar False
  let pconn = PlayerConnection conn queue closed char
  addConnection (gameClients gs) pconn
  return pconn

addConnection :: PlayerConnectionList -> PlayerConnection -> STM ()
addConnection (PlayerConnectionList connections) conn =
  do
    currentConnections <- readTVar connections
    writeTVar connections (conn : currentConnections)

removeConnection :: PlayerConnectionList -> PlayerConnection -> STM ()
removeConnection (PlayerConnectionList connections) conn =
  do
    currentConnections <- readTVar connections
    let newConnections = filter (/= conn) currentConnections
    writeTVar connections newConnections

getConnections :: PlayerConnectionList -> STM [PlayerConnection]
getConnections (PlayerConnectionList connectionList) =
  readTVar connectionList

newConnectionList :: STM PlayerConnectionList
newConnectionList = do
  l <- newTVar []
  return $ PlayerConnectionList l

queueCommand :: PlayerConnection -> Command -> STM ()
queueCommand conn cmd =
  writeTBQueue (connectionQueue conn) cmd

sendLn :: Conn -> B.ByteString -> IO ()
sendLn conn str = sendToClient conn (B.concat [str, fromString "\n"])

