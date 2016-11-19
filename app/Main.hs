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

import Login
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

clientLoop :: GameState -> PlayerConnection -> IO ()
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
