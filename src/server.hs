import Network
import Control.Concurrent
import Control.Exception
import System.IO
import Data.Text

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 4000
    loop sock

loop sock = do
    (h, _, _) <- accept sock
    forkIO $ body h
    loop sock
   where
    body h = do
        client_play_game h
        putStrLn "Disconnecting a client."
        hFlush h
        hClose h

client_play_game h = do
    hPutStrLn h "Welcome to Dirty Water, friend."
    name <- client_query_name h
    client_loop h

client_query_name h = do
    hPutStrLn h "What name would you like to be referred to by?"
    line <- hGetLine h
    let name = unpack $ strip $ pack line
    hPutStrLn h $ "Ah, " ++ name ++ ", an excellent name! Are you sure that is what you want to go by? (y/n)"
    confirm_name name
   where
    confirm_name name = do
        line <- hGetLine h
        case line of
            'y' : _ -> name
            'n' : _ -> do
                hPutStrLn h "Changed your mind already, eh?"
                client_query_name h
            _ -> do
                hPutStrLn h $ "I said, \"(y/n),\" not whatever crap you typed."
                hPutStrLn h $ "Let's try this again. Are you sure you want to go by " ++ name ++ "?"
                confirm_name name

client_loop h = do
    result <- try $ hWaitForInput h 1000 :: IO (Either IOError Bool)
    case result of
        Left _ -> putStrLn "Client disconnected"
        Right ready ->
            if ready then do
                line <- hGetLine h
                let str = unpack $ strip $ pack line
                if str == "exit" then
                    hPutStr h "Good bye\n"
                else do
                    hPutStr h $ "I don't understand \"" ++ str ++ "\"\n"
                    client_loop h
            else do
                putStrLn "Waiting for input"
                client_loop h
