import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Text
import Database.HDBC
import Database.HDBC.Sqlite3
import Network
import System.IO

import Character
import Command
import Connection
import Location
import State
import Types

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting server."
  let port = 4000
  sock <- listenOn $ PortNumber port
  putStrLn $ "Listening on port " ++ show port ++ "."
  connections <- atomically $ newConnectionList
  gameState <- newGameState "mud.db" connections
  _ <- forkIO (mainServer gameState)
  forever $ do
    (h, _, _) <- accept sock
    putStrLn "Got a new connection."
    queue <- atomically $ newTBQueue 10
    idVar <- newEmptyMVar
    closed <- atomically $ newTVar False
    startLoc <- atomically $ newLocation "Starting Area" "Insert description here"
    char <- atomically $ newCharacter "New Character" (ContainerLocation startLoc)
    let conn = ClientConnection h queue closed char idVar
    atomically $ addConnection connections conn
    tId <- forkFinally (clientPlayGame gameState conn)  (cleanupClient h)
    putMVar idVar tId

mainServer :: GameState -> IO ()
mainServer gameState = do
  processCommands gameState
  threadDelay 100000
  mainServer gameState

cleanupClient :: Handle -> Either SomeException () -> IO ()
cleanupClient h _ = do
  putStrLn "Disconnecting a client."
  hClose h

clientPlayGame :: GameState -> ClientConnection -> IO ()
clientPlayGame gs conn = do
  hPutStrLn (connectionHandle conn) "Welcome to Dirty Water, friend."
  name <- clientQueryName (connectionHandle conn)
  atomically $ changeName (connectionCharacter conn) name
  clientLoop gs conn

clientQueryName :: Handle -> IO String
clientQueryName h = do
  hPutStrLn h "What name would you like to be referred to by?"
  line <- hGetLine h
  let name = unpack $ strip $ pack line
  hPutStrLn h $ "Ah, " ++ name ++ ", an excellent name! Are you sure that is what you want to go by? (y/n)"
  confirmName name
  where
    confirmName :: String -> IO String
    confirmName name = do
      line <- hGetLine h
      case line of
        'y' : _ -> return name
        'n' : _ -> do
          hPutStrLn h "Changed your mind already, eh?"
          clientQueryName h
        _ -> do
          hPutStrLn h "I said, \"(y/n),\" not whatever crap you typed."
          hPutStrLn h $ "Let's try this again. Are you sure you want to go by " ++ name ++ "?"
          confirmName name

clientLoop :: GameState -> ClientConnection -> IO ()
clientLoop gs conn = do
  closed <- atomically $ readTVar (connectionClosed conn)
  let h = connectionHandle conn
  unless closed $ do
    l <- tryJust (guard . isExitException) $ hGetLine h
    case l of
      Left _ -> return ()
      Right line -> do
        let str = unpack $ strip $ pack line
        cl <- atomically $ readTVar $ commandList gs
        case parseCommand cl str of
          Left _ -> hPutStrLn h "Parse error"
          Right cmd -> atomically $ queueCommand conn cmd
    clientLoop gs conn
