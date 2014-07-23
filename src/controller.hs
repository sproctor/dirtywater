import Control.Concurrent.Chan
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import System.IO

data Command = Look | Exit | Move Direction deriving (Show, Eq)

data Direction = North | East | South | West deriving (Show, Eq)

data Character = Pc | Npc

data Connection = Connection { handle :: Handle, queue :: TBQueue Command, character :: Character }

data ServerStatus = Running | Stopping

data GameState = GameState { connections :: [Connection], status :: ServerStatus }

{- class Controller c where
  getCommand :: c -> IO (Maybe Command)
  putOutput :: c -> String -> IO ()
  getCharacter :: c -> Character

instance Controller Connection where
-}
getCommand :: Connection -> IO (Maybe Command)
getCommand (Connection _ q _) =
  atomically (tryReadTBQueue q)
putOutput :: Connection -> String -> IO ()
putOutput (Connection h _ _) =
  hPutStr h
-- getCharacter :: Connection -> Character
-- getCharacter (Connection _ _ c) = c

mainServer gameState = do
  let (connection : remainingConnections) = connections gameState
  cmd <- getCommand connection
  let
    newConnections :: [Connection]
    newConnections = remainingConnections ++ [connection]
    newGameState =
      case cmd of
        Just realCommand ->
          --do
          let
            cs =
              if realCommand /= Exit
                then newConnections
                else remainingConnections
            --print ((getCharacter connection), realCommand)
          in
            doCommand (character connection) realCommand (GameState cs (status gameState))
        Nothing -> GameState newConnections (status gameState)
  mainServer newGameState

doCommand :: Character -> Command -> GameState -> GameState
doCommand _ _ gs = gs
