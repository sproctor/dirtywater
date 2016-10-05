{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

instance Show Command where
  show (Command (cmd, args, _)) = cmd ++ (show args)
  show (BadCommand s) = "Bad command: " ++ s

newtype CommandDef = CommandDef (String, [CommandArgsType], GameState -> ClientConnection -> CommandArgs -> IO ())

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
  deriving (Eq, Show, Enum, Bounded)

data WeaponType
  = WeaponBroadsword
  | WeaponShortsword
  | WeaponNone
  deriving (Eq, Show, Enum, Bounded)

data Character =
  Character
    { charContainer :: TVar Container
    , charName :: TVar String
    , charPassword :: TVar String
    , charHands :: TVar [ItemSlot]
    , charSlots :: TVar [ItemSlot]
    , charInventory :: TVar [Item]
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
  deriving (Eq, Enum, Bounded)

instance Show Direction where
  show East = "east"
  show North = "north"
  show Northeast = "northeast"
  show Northwest = "northwest"
  show South = "south"
  show Southeast = "southeast"
  show Southwest = "southwest"
  show West = "west"

instance Read Direction where
  readsPrec _ value =
    tryParse [(minBound :: Direction) ..]
    where
      tryParse [] = []
      tryParse (result:xs) =
        let attempt = show result in
        if (take (length attempt) value) == attempt
          then [(result, drop (length attempt) value)]
          else tryParse xs

newtype LocationId = LocationId Int deriving (Num, Show, Eq, Read, Enum, Real, Ord, Integral)

data Location =
  Location
  { locationId :: LocationId
  , locationTitle :: String
  , locationDesc :: String
  , locationObjects :: TVar [Object]
  } deriving Eq

data Portal
  = Portal { portalDestA :: LocationId, portalDestB :: LocationId } deriving (Eq, Show)

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
