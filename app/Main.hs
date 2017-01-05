{-# LANGUAGE OverloadedStrings #-}

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

import Login
import ParseCommand
import Command
import Connection
import Location
import State
import Tangible
import Types

main :: IO ()
main = do
  let port = 4000
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
  charecter <- clientHandshakeChar gs conn
  pconn <- atomically $ newConnection gs conn charecter
  cmdLook gs pconn CmdArgsNone
  cPutStr pconn ">"
  clientLoop gs pconn
  loc <- atomically $ getLocation charecter
  atomically $ locationRemoveObject loc (ObjectCharacter charecter)

mainServer :: GameState -> IO ()
mainServer gs =
  -- let dbconn = sqlConnection gs in
  forever $ do
    processCommands gs
    -- commit dbconn
    threadDelay 100000

clientLoop :: GameState -> PlayerConnection -> IO ()
clientLoop gs pconn = do
  closed <- atomically $ readTVar (connectionClosed pconn)
  unless closed $ do
    possibleLine <- tryJust (guard . isExitException) $ cGetLine pconn
    case possibleLine of
      Left _ -> saveCharacter (sqlConnection gs) (connectionCharacter pconn)
      Right line ->
        case parseCommand (commandList gs) (strTrim line) of
          Left e -> cPutStrLn pconn $ "Parse error at " `B.append` fromString (show e)
          Right cmd -> atomically $ queueCommand pconn cmd
    clientLoop gs pconn

