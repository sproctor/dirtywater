open Lua_api
open Types
open Debug

let run_location_script (loc : location) (script : string) =
  let ls = LuaL.newstate () in
  LuaL.openlibs ls;
  if LuaL.dostring ls script then
    dlog 4 "ran script"
  else
    dlog 0 ("Script failed!! script: " ^ script)
