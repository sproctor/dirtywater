{-# LANGUAGE OverloadedStrings #-}
import qualified Scripting.Lua as Lua
import qualified Foreign.C.Types as C
import Control.Monad

hello :: Lua.LuaState -> IO C.CInt
hello state = do
  isfunction <- Lua.isfunction state (-1)
  when isfunction $ Lua.call state 0 1
  isstring <- Lua.isstring state (-1)
  if isstring then do
    str <- Lua.tostring state (-1)
    putStrLn $ "Hello, " ++ str
  else
    putStrLn $ "Sorry, invalid call"
  Lua.pop state 1
  return 0

getString :: Lua.LuaState -> IO String
getString state = do
  isFunction <- Lua.isfunction state (-1)
  if isFunction then do
    putStrLn "calling function"
    Lua.call state 0 1
    getString state
  else do
    isString <- Lua.isstring state $ -1
    if isString then
      Lua.tostring state $ -1
    else do
      ltype <- Lua.ltype state 1
      typename <- Lua.typename state ltype
      putStrLn $ "Invalid type: " ++ typename
      return $ "Invalid type: " ++ typename

getValue :: Lua.LuaState -> String -> IO String
getValue state key = do
  isTable <- Lua.istable state $ -1
  if isTable then do
    Lua.getfield state (-1) key
    str <- getString state
    Lua.pop state 1
    return str
  else return "top of stack is not a table"

addLocation :: Lua.LuaState -> IO C.CInt
addLocation state = do
  name <- getValue state "name"
  putStrLn $ "Name: " ++ name
  description <- getValue state "description"
  putStrLn $ "Description: " ++ description
  return 0

main = do
  state <- Lua.newstate
  -- Lua.registerrawhsfunction state "hello" hello
  -- Lua.registerrawhsfunction state "addLocation" addLocation
  -- _ <- Lua.loadfile state "test.lua"
  -- Lua.call state 0 0
  Lua.newtable state
  str <- Lua.tostring state (-1)
  putStrLn $ "String: " ++ str
  Lua.close state
