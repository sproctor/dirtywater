import Network
import Control.Concurrent
import Control.Exception
import System.IO
import Data.Text

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 4000
  loop sock

loop :: Socket -> IO ()
loop sock = forever $ do
  (h, _, _) <- accept sock
  forkFinally (clientPlayGame h)  (cleanupClient h)

cleanupClient :: Handle -> (Either SomeException ()) -> IO ()
cleanupClient h _ = do
  putStrLn "Disconnecting a client."
  hFlush h
  hClose h

clientPlayGame :: Handle -> IO ()
clientPlayGame h = do
  hPutStrLn h "Welcome to Dirty Water, friend."
  name <- clientQueryName h
  clientLoop h

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

clientLoop :: Handle -> IO ()
clientLoop h = do
  hPutStr h ">"
  line <- hGetLine h
  let str = unpack $ strip $ pack line
  if str == "exit"
  then hPutStrLn h "Good bye"
  else do hPutStrLn h $ "I don't understand \"" ++ str ++ "\""
          clientLoop h
