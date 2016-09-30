import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Text
import Database.HDBC
import Database.HDBC.Sqlite3
import Network
import System.Directory
import System.IO
import System.Timeout

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
  gs <- newGameState "mud.db" connections
  -- Process game commands and update game state in a new thread
  _ <- forkIO (mainServer gs)
  -- Accept incoming connections
  acceptLoop gs sock

acceptLoop :: GameState -> Socket -> IO ()
acceptLoop gs sock = do
  state <- atomically $ readTVar $ gameStatus gs
  case state of
    Running -> do
      r <- timeout 100000 $ accept sock
      case r of
        Just (h, _, _) -> do
          putStrLn "Got a new connection."
          idVar <- newEmptyMVar
          tId <- forkFinally (clientPlayGame gs h idVar)  (cleanupClient h)
          putMVar idVar tId
          acceptLoop gs sock
        Nothing -> acceptLoop gs sock
    Stopping -> putStrLn "Shutting down server"

mainServer :: GameState -> IO ()
mainServer gs =
  let dbconn = sqlConnection gs in
  forever $ do
    processCommands gs
    commit dbconn
    threadDelay 100000

cleanupClient :: Handle -> Either SomeException () -> IO ()
cleanupClient h _ = do
  putStrLn "Disconnecting a client."
  hClose h

clientPlayGame :: GameState -> Handle -> MVar ThreadId -> IO ()
clientPlayGame gs h idVar = do
  char <- clientHandshakeChar gs h
  conn <- atomically $ newConnection gs h char idVar
  cmdLook gs conn CmdArgsNone
  putOutput conn ">"
  clientLoop gs conn

clientHandshakeChar :: GameState -> Handle -> IO Character
clientHandshakeChar gs h = do
  name <- clientQueryName h
  case name of
    "new" -> do
      newName <- clientCreateName h
      password <- clientCreatePassword h
      newCharacter gs newName password
    _ -> do
      chars <- atomically $ readTVar $ gameCharacters gs
      mc <- atomically $ findCharacter name gs
      case mc of
        Just c -> do
          clientQueryPassword h c
          return c
        Nothing -> do
          hPutStrLn h $ "You must be mistaken. There is no one named \"" ++ name ++ ".\" Let's try this again."
          clientHandshakeChar gs h


clientQueryName :: Handle -> IO String
clientQueryName h = do
  hPutStrLn h "Welcome to Dirty Water, friend. What name do you go by? If you are new here, type \"new\"."
  line <- hGetLine h
  let name = unpack $ strip $ pack line
  return name

clientQueryPassword :: Handle -> Character -> IO ()
clientQueryPassword h char = do
  name <- atomically $ readTVar $ charName char
  password <- atomically $ readTVar $ charPassword char
  hPutStrLn h $ "Welcome back, " ++ name ++ ". Please enter your password."
  line <- hGetLine h
  let supposedPassword = unpack $ strip $ pack line
  if password == supposedPassword
    then return ()
    else do
      hPutStrLn h $ "What are you trying to pull here, \"" ++ name ++ "?\" Maybe I should call you liar instead? Let's start this over."
      clientQueryPassword h char

clientCreateName :: Handle -> IO String
clientCreateName h = do
  hPutStrLn h "Would name would you like to be referred to by?"
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

clientCreatePassword :: Handle -> IO String
clientCreatePassword h = do
  hPutStrLn h "Please enter a password."
  line <- hGetLine h
  let password = unpack $ strip $ pack line
  hPutStrLn h "Please re-enter your password to verify."
  line <- hGetLine h
  let password2 = unpack $ strip $ pack line
  if password == password2
    then return password
    else do
      hPutStrLn h "Your passwords did not match. Let's try this again."
      clientCreatePassword h

clientLoop :: GameState -> ClientConnection -> IO ()
clientLoop gs conn = do
  closed <- atomically $ readTVar (connectionClosed conn)
  unless closed $ do
    let h = connectionHandle conn
    possibleLine <- tryJust (guard . isExitException) $ hGetLine h
    case possibleLine of
      Left _ -> return ()
      Right line -> do
        let strippedLine = unpack $ strip $ pack line
        commands <- atomically $ readTVar $ commandList gs
        case parseCommand commands strippedLine of
          Left e -> hPutStrLn h $ "Parse error at " ++ (show e)
          Right cmd -> atomically $ queueCommand conn cmd
    clientLoop gs conn

