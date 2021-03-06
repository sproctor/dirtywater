{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Concurrent.STM
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as B
import Data.Typeable
import Database.HDBC.Sqlite3 as SQL
import Data.Conduit.Network.Server

data NounOrd
  = NounAny
  | NounOrd Int
  deriving (Show, Eq)

newtype AdjectiveList = AdjectiveList [ByteString] deriving (Show, Eq)

newtype Noun = Noun ByteString deriving (Show, Eq)

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
  | CmdTypeList [CommandArgsType]
  | CmdTypeNone
  deriving (Show, Eq)

data CommandArgs
  = CmdArgsObjectDesc ObjectDesc
  | CmdArgsString ByteString
  | CmdArgsList [CommandArgs]
  | CmdArgsNone
  deriving (Show, Eq)

data Command
  = Command String CommandArgs (GameState -> Character -> CommandArgs -> IO ())
  | BadCommand ByteString

instance Eq Command where
  (Command cmd1 args1 _) == (Command cmd2 args2 _) =
    cmd1 == cmd2 && args1 == args2

instance Show Command where
  show (Command cmd args _) = cmd ++ (show args)
  show (BadCommand s) = "Bad command: " ++ B.toString s

data CommandDef = CommandDef String [CommandArgsType] (GameState -> Character -> CommandArgs -> IO ())

newtype Volume = Volume Int deriving (Ord, Eq, Show, Read)

newtype ItemTemplateId = ItemTemplateId String deriving Eq

instance Show ItemTemplateId where
  show (ItemTemplateId str) = "itp:" ++ str

data ItemTemplate =
  ItemTemplate
    { itemTemplId :: ItemTemplateId
    , itemTemplName :: ByteString
    , itemTemplAdjs :: [ByteString]
    , itemTemplShortDesc :: VisibleProperty
    , itemTemplLongDesc :: VisibleProperty
    , itemTemplWeaponType :: SkillId -- TODO: Add a list somewhere of valid weapon types to avoid typos
    }

newtype ItemId = ItemId Int deriving (Eq, Num)

instance Show ItemId where
  show (ItemId n) = "itm:" ++ show n

data Item =
  Item
    { itemId :: ItemId
    , itemContainer :: TVar Container
    , itemSlots :: [ItemSlot]
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
  deriving Eq

data ItemType
  = ItemAny
  | ItemHands
  | ItemHead
  | ItemLegs
  | ItemArms
  | ItemFeet
  | ItemTorso
  | ItemWrist -- for wrist sheaths
  | ItemAnkle -- for ankle sheaths
  | ItemForearm -- for shields
  deriving (Eq, Show, Enum, Bounded)

data AttackType = Thrust | Swing

data Attribute = Strength | Dexterity | Intelligence | Health deriving (Bounded, Enum, Eq)

instance Show Attribute where
  show Strength = "ST"
  show Dexterity = "DX"
  show Intelligence = "IQ"
  show Health = "HT"

instance Read Attribute where
  readsPrec _ value =
    tryParse [(minBound :: Attribute) ..]
    where
      tryParse [] = []
      tryParse (result:xs) =
        let attempt = show result in
        if (take (length attempt) value) == attempt
          then [(result, drop (length attempt) value)]
          else tryParse xs


data Character =
  Character
    { charId :: String
    , charConnection :: TVar CharacterConnection
    , charShortDescription :: Character -> IO ByteString
    , charLongDescription :: Character -> IO ByteString
    , charContainer :: TVar Container
    , charPassword :: TVar ByteString
    , charHolding :: [ItemSlot]
    , charInventory :: [ItemSlot]
    , charAttributes :: [(Attribute, TVar Int)]
    , charHP :: TVar Int
    , charSkills :: TVar [Skill]
    }

instance Eq Character where
  (==) a b = (charId a) == (charId b)

instance Show Character where
  show c = "chr:" ++ (show . charId) c

newtype MobId = MobId Int deriving (Eq, Num)

instance Show MobId where
  show (MobId n) = "mob:" ++ show n

newtype MobTemplateId = MobTemplateId String deriving Eq

instance Show MobTemplateId where
  show (MobTemplateId str) = "mtp:" ++ str

data MobTemplate =
  MobTemplate
    { mobTemplateId :: MobTemplateId
    , mobTemplateName :: String
    , mobTemplateAdjs :: [String]
    }

newtype SkillId = SkillId String deriving Eq

data Skill =
  Skill
    { skillId :: SkillId
    , skillRank :: Int
    }

data SkillDifficulty = SkillEasy | SkillAverage | SkillHard | SkillVeryHard deriving (Bounded, Enum, Eq)

instance Show SkillDifficulty where
  show SkillEasy = "E"
  show SkillAverage = "A"
  show SkillHard = "H"
  show SkillVeryHard = "VH"

instance Read SkillDifficulty where
  readsPrec _ value =
    tryParse [(minBound :: SkillDifficulty) ..]
    where
      tryParse [] = []
      tryParse (result:xs) =
        let attempt = show result in
        if (take (length attempt) value) == attempt
          then [(result, drop (length attempt) value)]
          else tryParse xs

data SkillDefault = SkillDefaultByAttribute Attribute Int | SkillDefaultBySkill String Int

data SkillDef =
  SkillDef
    { skillDefId :: SkillId
    , skillDefName :: ByteString
    , skillDefDifficulty :: SkillDifficulty
    , skillDefDefault :: [SkillDefault]
    }

data Object
  = ObjectItem Item
  | ObjectCharacter Character
  | ObjectDirection Direction Portal
  deriving Eq

data Container
  = ContainerLocation Location
  | ContainerItem Item
  | ContainerCharacter Character
  deriving (Eq)

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
  = StaticVisibleProperty ByteString
  | DynamicVisibleProperty (Character -> IO ByteString)

showVisibleProperty :: Character -> VisibleProperty -> IO ByteString
showVisibleProperty _ (StaticVisibleProperty str) = return str
showVisibleProperty c (DynamicVisibleProperty f) = f c

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
  { gameUsers :: UserConnectionList
  , gameStatus :: TVar ServerStatus
  , sqlConnection :: Connection
  , gameCommandList :: [CommandDef]
  , gameLocations :: [Location]
  , gameNextItemId :: TVar ItemId
  , gameNextMobId :: TVar MobId
  , gameItemTemplates :: [ItemTemplate]
  --, gameMobTemplates :: [MobTemplate]
  , gameSkillDefs :: [SkillDef]
  }

data CharacterConnection
    = PlayerConnection UserConnection
    | NoCharacterConnection

-- TODO: Can we find a better name for this one?
type Conn = ClientConnection B.ByteString B.ByteString

data UserConnection = UserConnection
    { userConnection :: Conn
    , userCommandQueue :: TBQueue Command
    , userDisconnected :: TVar Bool
    , userCharacter :: Character
    }

instance Eq UserConnection where
  p1 == p2 = userCharacter p1 == userCharacter p2

type UserConnectionList = TVar [UserConnection]

data InvalidValueException =
  InvalidValueException String
  deriving (Show, Typeable)

instance Exception InvalidValueException
