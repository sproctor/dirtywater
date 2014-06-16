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
        hPutStr h msg
        client_loop h
        putStrLn "Disconnecting a client."
        hFlush h
        hClose h

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

msg = "Welcome to dirty water\n"
