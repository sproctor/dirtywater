module LuaHelpers where

import Control.Monad
import Control.Monad.Extra
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
-- import Debug.Trace (trace)
import Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua
import System.Directory
import System.FilePath

import Types

{-
getLuaGlobalInt :: LuaState -> String -> IO Int
getLuaGlobalInt luaState key = do
  _ <- Lua.getglobal luaState key
  result <- fmap fromIntegral $ Lua.tointeger luaState (-1)
  Lua.pop luaState 1
  -- Set the global to nil so it can't be used accidentally in the future.
  Lua.pushnil luaState
  Lua.setglobal luaState key
  return result
-}

getLuaGlobalString :: LuaState -> String -> IO ByteString
-- getLuaGlobalString _ key | trace ("getLuaGlobalString luaState " ++ key) False = undefined
getLuaGlobalString luaState key = do
  Lua.getglobal luaState key
  -- unlessM (Lua.isstring luaState (-1)) $ throwIO $ InvalidValueError $ "\"" ++ key "\" must be a string."
  result <- Lua.tostring luaState (-1)
  Lua.pop luaState 1
  -- Set the global to nil so it can't be used accidentally in the future.
  Lua.pushnil luaState
  Lua.setglobal luaState key
  return result

getLuaGlobalStringWithDefault :: LuaState -> String -> ByteString -> IO ByteString
-- getLuaGlobalStringWithDefault _ key defaultStr | trace ("getLuaGlobalString luaState " ++ key ++ " " ++ (UTF8.toString defaultStr)) False = undefined
getLuaGlobalStringWithDefault luaState key defaultStr = do
  Lua.getglobal luaState key
  result <- ifM (Lua.isstring luaState (-1)) (Lua.tostring luaState (-1)) (return defaultStr)
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
-- loadLuaFile _ _ objId file | trace ("loadLuaFile luaState f " ++ objId ++ " " ++ show file) False = undefined
loadLuaFile luaState loadFun objId file = do
  startstacksize <- Lua.gettop luaState
  -- putStrLn $ "Stack size: " ++ show startstacksize
  status <- Lua.loadfile luaState file
  if status == 0
    then do
      Lua.call luaState 0 Lua.multret
      result <- loadFun luaState objId
      endstacksize <- Lua.gettop luaState
      when (startstacksize /= endstacksize) (error $ "Stack size changed when processing " ++ show file)
      return result
    else do
      putStrLn "Got an error!"
      errMsg <- Lua.tostring luaState (-1)
      error $ "ERROR: " ++ (UTF8.toString errMsg)

pushCharacter :: LuaState -> Character -> Character -> IO ()
pushCharacter luaState target viewer = do
  Lua.newtable luaState
  Lua.pushstring luaState (UTF8.fromString (charId target))
  Lua.setfield luaState (-2) "id"
  shortDescription <- charShortDescription target viewer
  Lua.pushstring luaState shortDescription
  Lua.setfield luaState (-2) "shortDescription"
  longDescription <- charLongDescription target viewer
  Lua.pushstring luaState longDescription
  Lua.setfield luaState (-2) "longDescription"

getPropertyForCharacter :: LuaState -> String -> Character -> IO ByteString
getPropertyForCharacter luaState objId c = do
  Lua.getglobal luaState "_properties"
  Lua.getfield luaState (-1) objId
  unlessM (Lua.isfunction luaState (-1)) $ error $ "Invalid value in _properties[" ++ objId ++ "]"
  pushCharacter luaState c c
  Lua.call luaState 1 1
  result <- Lua.tostring luaState (-1)
  Lua.pop luaState 1
  return result

getLuaGlobalVisibleProperty :: LuaState -> String -> String -> IO VisibleProperty
-- getLuaGlobalVisibleProperty _ objId key | trace ("getLuaGlobalVisibleProperty luaState " ++ objId ++ " " ++ key) False = undefined
getLuaGlobalVisibleProperty luaState objId key = do
  Lua.getglobal luaState key
  ifM (Lua.isfunction luaState (-1))
    (do
      Lua.getglobal luaState "_properties"
      propertiesType <- Lua.ltype luaState (-1)
      when (propertiesType /= Lua.TTABLE) $ do
        Lua.pop luaState 1
        Lua.newtable luaState
        Lua.setglobal luaState "_properties"
        Lua.getglobal luaState "_properties"
      Lua.insert luaState (-2)
      Lua.setfield luaState (-2) objId
      Lua.pop luaState 1
      return $ DynamicVisibleProperty (getPropertyForCharacter luaState objId)
    )
    (do
      isString <- Lua.isstring luaState (-1)
      if isString
        then do
          value <- Lua.tostring luaState (-1)
          Lua.pop luaState 1
          return $ StaticVisibleProperty value
        else error $ "Property (" ++ key ++ ") has invalid type"
    )
