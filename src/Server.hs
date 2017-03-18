{-# LANGUAGE OverloadedStrings #-}

module Server (
  runDirtywaterServer
  ) where

import Prelude hiding (putStrLn)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.String.Class
import Data.Strings hiding (fromString)
import Database.HDBC
import Data.Conduit.Network.Server
import Control.Concurrent.Async (concurrently)
import Data.Attoparsec.ByteString (Parser, takeTill)
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, endOfLine)

import Character
import Command
import Location
import Login
import ParseCommand
import State
import Tangible
import Types
import UserConnection

runDirtywaterServer :: Int -> IO ()
runDirtywaterServer port = do
  putStrLn $ "Starting server. Listening on port " ++ show port ++ "."
  connections <- atomically newConnectionList
  gs <- newGameState "mud.db" connections
  _ <- concurrently
    (mainServer gs)
    (serve port parseLine id (handleClient gs) reportError)
  putStrLn ("Shutting down server." :: String)
  commit (sqlConnection gs)

parseLine :: Parser B.ByteString
parseLine = takeTill isEndOfLine <* endOfLine

reportError :: Conn -> SomeException -> IO ()
reportError _conn exn = putStrLn $ "Caught exception: " ++ show exn

handleClient :: GameState -> Conn -> IO ()
handleClient gs conn = do
  putStrLn ("Got a new connection." :: String)
  character <- clientHandshakeChar gs conn
  user <- atomically $ newUserConnection gs conn character
  cmdLook gs character CmdArgsNone
  cPutStr character ">"
  clientLoop gs user
  loc <- atomically $ getLocation character
  atomically $ locationRemoveObject loc (ObjectCharacter character)

mainServer :: GameState -> IO ()
mainServer gs =
  -- let dbconn = sqlConnection gs in
  forever $ do
    processCommands gs
    -- commit dbconn
    threadDelay 100000

clientLoop :: GameState -> UserConnection -> IO ()
clientLoop gs user = do
  disconnected <- atomically $ readTVar (userDisconnected user)
  let conn = userConnection user
  unless disconnected $ do
    maybeLine <- getFromClient conn
    case maybeLine of
      Nothing -> saveCharacter (sqlConnection gs) (userCharacter user)
      Just line ->
        case parseCommand (gameCommandList gs) (strTrim line) of
          Left e -> sendToClient conn $ "Parse error at " `B.append` fromString (show e)
          Right cmd -> atomically $ queueCommand user cmd
    clientLoop gs user

