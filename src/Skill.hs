module Skill where

import Control.Exception (throwIO)
import Control.Monad (fmap)
import Control.Monad.Extra (ifM)
import qualified Data.ByteString.UTF8 as UTF8
-- import Debug.Trace (trace)
import Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua
import Text.Read (readMaybe)

import LuaHelpers
import Types

getSkillDefaults :: LuaState -> String -> IO [SkillDefault]
-- getSkillDefaults _ key | trace ("getSkillDefaults luaState " ++ key) False = undefined
getSkillDefaults luaState key = do
  Lua.getglobal luaState key
  ifM (Lua.istable luaState (-1))
    (do
      n <- Lua.objlen luaState (-1)
      -- putStrLn $ "Table size: " ++ show n
      result <- mapM (getSkillDefault luaState) [1..n]
      Lua.pop luaState 1
      return result)
    (throwIO $ InvalidValueException $ "Global \"" ++ key ++ "\" must be a table.")

luaToAttribute :: LuaState -> Int -> IO Attribute
-- luaToAttribute _ index | trace ("luaToAttribute luaState " ++ show index) False = undefined
luaToAttribute luaState index = do
  str <- fmap UTF8.toString $ Lua.tostring luaState index
  case readMaybe str of
    Just attribute -> return attribute
    Nothing -> throwIO $ InvalidValueException $ "Invalid attribute string \"" ++ str ++ "\""

getSkillDefault :: LuaState -> Int -> IO SkillDefault
-- getSkillDefault _ i | trace ("getSkillDefault luaState " ++ show i) False = undefined
getSkillDefault luaState i = do
  Lua.rawgeti luaState (-1) i
  Lua.getfield luaState (-1) "modifier"
  modifier <- fmap fromIntegral $ Lua.tointeger luaState (-1)
  Lua.pop luaState 1
  Lua.getfield luaState (-1) "attribute"
  isAttribute <- fmap not $ Lua.isnil luaState (-1)
  if isAttribute
    then do
      attribute <- luaToAttribute luaState (-1)
      Lua.pop luaState 2
      return $ SkillDefaultByAttribute attribute modifier
    else do
      Lua.pop luaState 1
      _ <- Lua.getfield luaState (-1) "skill"
      isSkill <- fmap not $ Lua.isnil luaState (-1)
      if isSkill
        then do
          skill <- fmap UTF8.toString $ Lua.tostring luaState (-1)
          Lua.pop luaState 2
          return $ SkillDefaultBySkill skill modifier
        else do
          Lua.pop luaState 2
          throwIO $ InvalidValueException "Skill default definition must include either an attribute or skill."

getDifficulty :: LuaState -> String -> IO SkillDifficulty
-- getDifficulty _ key | trace ("getDifficulty luaState " ++ key) False = undefined
getDifficulty luaState key = do
  str <- fmap UTF8.toString $ getLuaGlobalString luaState key
  case readMaybe str of
    Just difficulty -> return difficulty
    Nothing -> throwIO $ InvalidValueException $ "Invalid value in difficulty: " ++ str

loadSkillDefinition :: LuaState -> String -> IO SkillDef
-- loadSkillDefinition _ skillId | trace ("loadSkillDefinition luaState " ++ skillId) False = undefined
loadSkillDefinition luaState skillId = do
  name <- getLuaGlobalString luaState "name"
  difficulty <- getDifficulty luaState "difficulty"
  defaults <- getSkillDefaults luaState "default"
  return $ SkillDef (SkillId skillId) name difficulty defaults
