import Prelude hiding (putStrLn)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.String.Class
import Data.Strings
import Database.HDBC
import Database.HDBC.Sqlite3
import Network
import System.Directory
import System.IO (Handle, hClose, hSetEncoding, stderr, utf8)
import System.Timeout

import Character
import Command
import Connection
import Location
import State
import Tangible
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
  catch (acceptLoop gs sock) (\e -> putStrLn $ "Caught exception: " ++ show (e :: SomeException))
  putStrLn "Shutting down server."
  commit (sqlConnection gs)

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
          tId <- forkFinally (clientGameHandler gs h idVar)  (cleanupClient h)
          putMVar idVar tId
          acceptLoop gs sock
        Nothing -> acceptLoop gs sock
    Stopping -> return ()

mainServer :: GameState -> IO ()
mainServer gs =
  -- let dbconn = sqlConnection gs in
  forever $ do
    processCommands gs
    -- commit dbconn
    threadDelay 100000

cleanupClient :: Handle -> Either SomeException () -> IO ()
cleanupClient h _ = do
  putStrLn "Disconnecting a client."
  hClose h

clientPlayGame :: GameState -> Handle -> MVar ThreadId -> IO ()
clientPlayGame gs h idVar = do
  hSetEncoding h utf8
  char <- clientHandshakeChar gs h
  conn <- atomically $ newConnection gs h char idVar
  cmdLook gs conn CmdArgsNone
  cPutStr conn (">" :: String)
  clientLoop gs conn
  loc <- atomically $ getLocation char
  atomically $ locationRemoveObject loc (ObjectCharacter char)

clientGameHandler :: GameState -> Handle -> MVar ThreadId -> IO ()
clientGameHandler gs h idVar =
  catch (clientPlayGame gs h idVar)
    (\e -> do
        hPutStrLn stderr (show (e :: SomeException))
        hPutStrLn h $ "ERROR! " ++ show (e :: SomeException)
    )

clientHandshakeChar :: GameState -> Handle -> IO Character
clientHandshakeChar gs h = do
  name <- clientQueryName h
  if name == UTF8.fromString "new"
    then do
      newName <- clientCreateName h
      password <- clientCreatePassword h
      atomically $ newCharacter gs h newName password
    else do
      -- chars <- atomically $ readTVar $ gameCharacters gs
      mc <- loadCharacter gs h (UTF8.toString name)
      case mc of
        Just c -> do
          clientQueryPassword h c
          return c
        Nothing -> do
          hPutStrLn h $ "You must be mistaken. There is no one named \"" ++ UTF8.toString name ++ ".\" Let's try this again."
          clientHandshakeChar gs h


clientQueryName :: Handle -> IO ByteString
clientQueryName h = do
  hPutStrLn h "Welcome to Dirty Water\x00AE, friend. What name do you go by? If you are new here, type \"new\"."
  name <- B.hGetLine h
  return $ strTrim name

clientQueryPassword :: Handle -> Character -> IO ()
clientQueryPassword h char = do
  let name = UTF8.fromString $ charId char
  password <- atomically $ readTVar $ charPassword char
  hPutStrLn h $ "Welcome back, " ++ UTF8.toString name ++ ". Please enter your password."
  supposedPassword <- fmap strTrim $ B.hGetLine h
  if password == supposedPassword
    then return ()
    else do
      hPutStrLn h $ "What are you trying to pull here, \"" ++ UTF8.toString name ++ "?\" Maybe I should call you liar instead? Let's start this over."
      clientQueryPassword h char

clientCreateName :: Handle -> IO ByteString
clientCreateName h = do
  hPutStrLn h "Would name would you like to be referred to by?"
  name <- fmap strTrim $ B.hGetLine h
  hPutStrLn h $ "Ah, " ++ UTF8.toString name ++ ", an excellent name! Are you sure that is what you want to go by? (y/n)"
  confirmName name
  where
    confirmName :: ByteString -> IO ByteString
    confirmName name = do
      line <- fmap strTrim $ hGetLine h
      case line of
        'y' : _ -> return name
        'n' : _ -> do
          hPutStrLn h "Changed your mind already, eh?"
          clientQueryName h
        _ -> do
          hPutStrLn h "I said, \"(y/n),\" not whatever crap you typed."
          hPutStrLn h $ "Let's try this again. Are you sure you want to go by " ++ UTF8.toString name ++ "?"
          confirmName name

clientCreatePassword :: Handle -> IO ByteString
clientCreatePassword h = do
  hPutStrLn h "Please enter a password."
  password <- fmap strTrim $ B.hGetLine h
  hPutStrLn h "Please re-enter your password to verify."
  password2 <- fmap strTrim $ B.hGetLine h
  if password == password2
    then return password
    else do
      hPutStrLn h "Your passwords did not match. Let's try this again."
      clientCreatePassword h

clientLoop :: GameState -> ClientConnection -> IO ()
clientLoop gs conn = do
  closed <- atomically $ readTVar (connectionClosed conn)
  unless closed $ do
    possibleLine <- tryJust (guard . isExitException) $ cGetLine conn
    case possibleLine of
      Left _ -> saveCharacter (sqlConnection gs) (connectionCharacter conn)
      Right line -> do
        case parseCommand (commandList gs) (strTrim line) of
          Left e -> cPutStrLn conn $ "Parse error at " ++ (show e)
          Right cmd -> atomically $ queueCommand conn cmd
    clientLoop gs conn
