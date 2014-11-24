module Types where

import Control.Concurrent
import Control.Concurrent.STM
import Database.HDBC.Sqlite3
import System.IO

data NounOrd
  = NounAny
  | NounOrd Int
  deriving (Show, Eq)

newtype AdjectiveList = AdjectiveList [String] deriving (Show, Eq)

newtype Noun = Noun String deriving (Show, Eq)

newtype NounDesc = NounDesc (NounOrd, AdjectiveList, Noun) deriving (Show, Eq)

data Preposition
  = PrepOn
  | PrepIn
  | PrepBehind
  | PrepUnder
  | PrepAny
  deriving (Show, Eq)

data ObjectDesc
  = ObjectDesc (ObjectDesc, Preposition, NounDesc)
  | ObjectDescBase NounDesc
  deriving (Show, Eq)

data ExitDesc
  = ExitDirection Direction
  | ExitObject ObjectDesc

data CommandArgsType
  = CmdTypeObjectDesc
  | CmdTypeString
  | CmdTypeNone
  deriving (Show, Eq)

data CommandArgs
  = CmdArgsObjectDesc ObjectDesc
  | CmdArgsString String
  | CmdArgsNone
  deriving (Show, Eq)

data Command
  = Command (String, CommandArgs, GameState -> ClientConnection -> CommandArgs -> IO ())
  | BadCommand String

newtype CommandDef = CommandDef (String, [CommandArgsType], GameState -> ClientConnection -> CommandArgs -> IO ())

data Position = In | On deriving Eq

newtype Volume = Volume Int deriving (Ord, Eq, Show, Read)

data Item =
    Item {
      itemContainer :: TVar Container,
      itemName :: String,
      itemAdjs :: [String],
      itemContainers :: [(Position, Volume)],
      itemContainedItems :: [(Position, Item)],
      itemContainedChars :: [(Position, Character)]
    } deriving Eq

data Character =
    Character {
      charContainer :: TVar Container,
      charName :: TVar String
      --leftHand :: Item,
      --rightHand :: Item
    } deriving Eq

data Container = ContainerLocation Location | ContainerItem Item

data Direction
  = North
  | Northeast
  | East
  | Southeast
  | South
  | Southwest
  | West
  | Northwest
  deriving (Show, Eq)

data Location =
  Location {
    locationId :: Int,
    locationTitle :: String,
    locationDesc :: String,
    locationPortals :: TVar [Portal],
    locationChars :: TVar [Character],
    locationItems :: TVar [Item]
  }

data Portal
  = ItemPortal { portalItem :: Item, portalDest :: Location }
  | DirectionPortal { directionDir :: Direction, directionDest :: Location }

data ServerStatus = Running | Stopping

data GameState =
  GameState
  { gameClients :: ClientConnectionList
  , gameStatus :: ServerStatus
  , sqlConnection :: Connection
  , commandList :: TVar [CommandDef]
  , gameLocations :: TVar [Location]
  }

data ClientConnection = ClientConnection
    { connectionHandle :: Handle
    , connectionQueue :: TBQueue Command
    , connectionClosed :: TVar Bool
    , connectionCharacter :: Character
    , connectionThreadId :: MVar ThreadId
    } deriving Eq

newtype ClientConnectionList = ClientConnectionList (TVar [ClientConnection])
