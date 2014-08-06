import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Text
import Network
import System.IO
import System.Timeout

import Controller

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting server."
  let port = 4000
  sock <- listenOn $ PortNumber port
  putStrLn $ "Listening on port " ++ (show port) ++ "."
  connections <- atomically $ newTVar []
  forkIO (mainServer (return (GameState connections Running)))
  forever $ do
    (h, _, _) <- accept sock
    putStrLn "Got a new connection."
    queue <- atomically $ newTBQueue 10
    idVar <- newEmptyMVar
    closed <- atomically $ newTVar False
    let conn = Connection h queue closed Pc idVar
    atomically $ addConnection connections conn
    id <- forkFinally (clientPlayGame conn)  (cleanupClient h)
    putMVar idVar id

cleanupClient :: Handle -> (Either SomeException ()) -> IO ()
cleanupClient h _ = do
  putStrLn "Disconnecting a client."
  hClose h

clientPlayGame :: Connection -> IO ()
clientPlayGame conn = do
  hPutStrLn (connectionHandle conn) "Welcome to Dirty Water, friend."
  name <- clientQueryName (connectionHandle conn)
  clientLoop conn

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
          hPutStrLn h $ "I said, \"(y/n),\" not whatever crap you typed."
          hPutStrLn h $ "Let's try this again. Are you sure you want to go by " ++ name ++ "?"
          confirmName name

commandList =
  [
    ("exit", Exit),
    ("look", Look)
  ]

lookupCommand :: String -> Maybe Command
lookupCommand name =
  let
    helper [] = Nothing
    helper ((str, command) : rest) =
      if str == name
        then Just command
        else helper rest
  in
    helper commandList

clientLoop :: Connection -> IO ()
clientLoop conn = do
  closed <- atomically $ readTVar (connectionClosed conn)
  if closed
    then return ()
    else do
      hPutStr (connectionHandle conn) ">"
      l <- tryJust (guard . isExitException) $ hGetLine (connectionHandle conn)
      case l of
        Left _ -> return ()
        Right line -> do
          let str = unpack $ strip $ pack line
          let command = lookupCommand str
          case command of
            Just cmd -> atomically $ writeTBQueue (connectionQueue conn) cmd
            Nothing -> return ()
      clientLoop conn
