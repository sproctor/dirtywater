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

data ItemTemplate =
  ItemTemplate
    { itemTemplId :: String
    , itemTemplName :: String
    , itemTemplAdjs :: [String]
    , itemTemplShortDesc :: String
    , itemTemplLongDesc :: String
    , itemTemplWeaponType :: WeaponType
    }

data Item =
  Item
    { itemId :: Int
    , itemContainer :: TVar Container
    , itemAddObject :: Object -> Bool
    , itemContents :: TVar [ItemSlot]
    , itemTemplate :: ItemTemplate
    }

data ItemSlot =
  ItemSlot
    { slotType :: ItemType
    , slotPosition :: Position
    , slotContents :: TVar [Item]
    }

data ItemType
  = ItemAny
  | ItemHands
  | ItemHead
  | ItemLegs
  | ItemShield
  | ItemTorso
  | ItemWeapon

data WeaponType
  = WeaponBroadsword
  | WeaponShortsword
  | WeaponNone
  deriving (Eq, Show, Enum)

data Character =
  Character
    { charContainer :: TVar Container
    , charName :: TVar String
    , charPassword :: TVar String
    , charHands :: TVar [Item]
    , charST :: TVar Int
    , charDX :: TVar Int
    , charIQ :: TVar Int
    , charHT :: TVar Int
    , charHP :: TVar Int
    , charWill :: TVar Int
    , charPer :: TVar Int
    , charCurrHP :: TVar Int
    , charSkills :: TVar [Skill]
    } deriving Eq

data Skill =
  Skill
    { skillName :: String
    , skillRank :: TVar Int
    }

data Object
  = ObjectItem Item
  | ObjectCharacter Character
  | ObjectDirection Direction Portal

data Container
  = ContainerLocation Location
  | ContainerItem Item
  | ContainerCharacter Character

data Direction
  = East
  | North
  | Northeast
  | Northwest
  | South
  | Southeast
  | Southwest
  | West
  deriving (Eq, Enum)

newtype LocationId = LocationId Int deriving (Show, Eq, Read)

data Location =
  Location
  { locationId :: LocationId
  , locationTitle :: String
  , locationDesc :: String
  , locationObjects :: TVar [Object]
  } deriving Eq

data Portal
  = Portal { portalDestA :: LocationId, portalDestB :: LocationId }

data ServerStatus = Running | Stopping

data GameState =
  GameState
  { gameClients :: ClientConnectionList
  , gameStatus :: TVar ServerStatus
  , sqlConnection :: Connection
  , commandList :: TVar [CommandDef]
  , gameLocations :: TVar [Location]
  , gameNextItemId :: TVar Int
  , gameItemTemplates :: TVar [ItemTemplate]
  , gameCharacters :: TVar [Character]
  }

data ClientConnection = ClientConnection
    { connectionHandle :: Handle
    , connectionQueue :: TBQueue Command
    , connectionClosed :: TVar Bool
    , connectionCharacter :: Character
    , connectionThreadId :: MVar ThreadId
    } deriving Eq

newtype ClientConnectionList = ClientConnectionList (TVar [ClientConnection])
