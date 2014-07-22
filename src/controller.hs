data Command = Look | Exit | Move Direction

data Direction = North | East | South | West

data Connection = Connection { handle :: Handle, queue :: TBQueue Command, character :: Character }

data GameState = Running | Stopping

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

connectedControlers :: Chan Controller

mainServer gameState = do
  let c = readChan connectedControlers
  cmd <- getCommand c
  let newGameState =
    case cmd of
      Just realCommand -> do
        print ((getCharacter c), realCommand)
        when isGameCmd $ doACommand (getCharacter c) realCommand gameState
        when (cmd /= Exit) $ writeChan connectedControlers c
      Nothing -> gameState
  mainServer controllers newGameState

doACommand :: Character -> Command -> GameState -> GameState
doACommand _ _ gs = gs
