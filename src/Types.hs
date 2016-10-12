{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Typeable
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

newtype ItemId = ItemId String deriving Eq

instance Show ItemId where
  show (ItemId str) = "itm:" ++ str

data Item =
  Item
    { itemId :: Int
    , itemContainer :: TVar Container
    , itemAddObject :: Object -> Bool
    , itemContents :: TVar [ItemSlot]
    , itemTemplate :: ItemTemplate
    }

instance Eq Item where
  (==) a b = (itemId a) == (itemId b)

instance Show Item where
  show = show . itemId

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
    { charId :: String
    , charShortDescription :: Character -> IO String
    , charLongDescription :: Character -> IO String
    , charContainer :: TVar Container
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
    }

instance Eq Character where
  (==) a b = (charId a) == (charId b)

instance Show Character where
  show c = "chr:" ++ (show . charId) c

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
  deriving (Show, Eq)

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

newtype LocationId = LocationId String deriving Eq

instance Show LocationId where
  show (LocationId str) = "loc:" ++ str

data VisibleProperty
  = StaticVisibleProperty String
  | DynamicVisibleProperty (Character -> IO String)

data Location =
  Location
  { locationId :: LocationId
  , locationTitle :: VisibleProperty
  , locationDescription :: VisibleProperty
  , locationObjects :: TVar [Object]
  }

instance Eq Location where
  (==) a b = (locationId a) == (locationId b)

instance Show Location where
  show = show . locationId

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

data InvalidValueException =
  InvalidValueException String
  deriving (Show, Typeable)

instance Exception InvalidValueException
