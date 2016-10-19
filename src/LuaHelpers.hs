module LuaHelpers where

import Control.Monad
import Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua
import System.Directory
import System.FilePath

import Types

{-
getLuaGlobalInt :: LuaState -> String -> IO Int
getLuaGlobalInt luaState key = do
  _ <- Lua.getglobal luaState key
  result <- liftM fromIntegral $ Lua.tointeger luaState (-1)
  Lua.pop luaState 1
  -- Set the global to nil so it can't be used accidentally in the future.
  Lua.pushnil luaState
  Lua.setglobal luaState key
  return result
-}

getLuaGlobalString :: LuaState -> String -> IO String
getLuaGlobalString luaState key = do
  _ <- Lua.getglobal luaState key
  result <- Lua.tostring luaState (-1)
  Lua.pop luaState 1
  -- Set the global to nil so it can't be used accidentally in the future.
  Lua.pushnil luaState
  Lua.setglobal luaState key
  return result

loadFiles :: LuaState -> FilePath -> (LuaState -> String -> IO a) -> IO [a]
loadFiles luaState path loadFun = do
  files <- getDirectoryContents path
  let luaFiles = filter ((== ".lua") . takeExtension) files
  mapM (\f -> loadLuaFile luaState loadFun (dropExtension f) (path ++ (pathSeparator : f))) luaFiles

loadLuaFile :: LuaState -> (LuaState -> String -> IO a) -> String -> FilePath -> IO a
loadLuaFile luaState createFromLuaFun objId file = do
  status <- Lua.loadfile luaState file
  if status == Lua.OK
    then do
      Lua.call luaState 0 0
      createFromLuaFun luaState objId
    else do
      errMsg <- Lua.tostring luaState (-1)
      error $ "ERROR: " ++ errMsg

pushCharacter :: LuaState -> Character -> Character -> IO ()
pushCharacter luaState target viewer = do
  Lua.newtable luaState
  Lua.pushstring luaState (charId target)
  Lua.setfield luaState (-2) "id"
  shortDescription <- charShortDescription target viewer
  Lua.pushstring luaState shortDescription
  Lua.setfield luaState (-2) "shortDescription"
  longDescription <- charLongDescription target viewer
  Lua.pushstring luaState longDescription
  Lua.setfield luaState (-2) "longDescription"

getPropertyForCharacter :: LuaState -> String -> Character -> IO String
getPropertyForCharacter luaState objId c = do
  _ <- Lua.getglobal luaState "_properties"
  fieldType <- Lua.getfield luaState (-1) objId
  unless (fieldType == Lua.TFUNCTION) $ error $ "Invalid value in _properties[" ++ objId ++ "]"
  pushCharacter luaState c c
  Lua.call luaState 1 1
  result <- Lua.tostring luaState (-1)
  Lua.pop luaState 1
  return result

getLuaGlobalVisibleProperty :: LuaState -> String -> String -> IO VisibleProperty
getLuaGlobalVisibleProperty luaState objId key = do
  t <- Lua.getglobal luaState key
  case t of
    Lua.TFUNCTION -> do
      propertiesType <- Lua.getglobal luaState "_properties"
      when (propertiesType /= Lua.TTABLE) $ do
        Lua.pop luaState 1
        Lua.newtable luaState
        _ <- Lua.setglobal luaState "_properties"
        void $ Lua.getglobal luaState "_properties"
      Lua.insert luaState (-2)
      Lua.setfield luaState (-2) objId
      Lua.pop luaState 1
      return $ DynamicVisibleProperty (getPropertyForCharacter luaState objId)
    _ -> do
      isString <- Lua.isstring luaState (-1)
      if isString
        then do
          value <- Lua.tostring luaState (-1)
          Lua.pop luaState 1
          return $ StaticVisibleProperty value
        else error $ "Property (" ++ key ++ ") has type: " ++ show t

