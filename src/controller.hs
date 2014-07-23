import Control.Concurrent.Chan
import Control.Concurrent.STM.TBQueue
import Control.Monad
import System.IO

data Command = Look | Exit | Move Direction

data Direction = North | East | South | West

data Character = Pc | Npc

data Connection = Connection { handle :: Handle, queue :: TBQueue Command , character :: Character }

data ServerStatus = Running | Stopping

data GameState = GameState { controllers :: [Controller], status :: ServerStatus }

class Controller c where
  getCommand :: c -> IO (Maybe Command)
  putOutput :: c -> String -> IO ()
  getCharacter :: c -> Character

instance Controller Connection where
  getCommand (Connection _ queue _) =
    if null queue
      then Nothing
      else Just (head queue)
  putOutput (Connection handle _ _) =
    hPutStr handle
  getCharacter (Connection _ _ c) = c

mainServer gameState = do
  let controller : remainingControllers = gameState.controllers
  cmd <- getCommand controller
  let
    newControllers :: [Controller]
    newControllers =
      if cmd == Exit
        then remainingControllers
        else remainingControllers ++ controller
    intermediateGameState = GameState newControllers status
    newGameState =
      case cmd of
        Just realCommand -> do
          print ((getCharacter controller), realCommand)
          doACommand (getCharacter controller) realCommand intermediateGameState
        Nothing -> intermediateGameState
  mainServer controllers newGameState

doACommand :: Character -> Command -> GameState -> GameState
doACommand _ _ gs = gs
