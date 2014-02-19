open Lua_api
open Types
open Base

let get_script_location (ls : Lua.state) (str : string) : location =
  Lua.getglobal ls str;
  match Lua.touserdata ls (-1) with
  | Some `Userdata l -> l
  | Some `Light_userdata l -> l
  | _ -> failwith "Expected location"

(* only valid for locations *)
let purge_location (ls : Lua.state) : int =
  print_endline "Purging";
  let loc = get_script_location ls "self" in
  print_endline ("got location " ^ (loc # to_string));
  0

let add_tangible (ls : Lua.state) : int =
  let argc = Lua.gettop ls in
  if argc < 1 then (
    dlog 0 "Not enough arguments to \"add_tangible\"";
    0
  ) else (
    let id = Option.get (Lua.tostring ls 1) in
    let loc = get_script_location ls "self" in
    let thing = Templates.put_tangible id (loc :> container) On in
    Lua.newuserdata ls thing;
    1
  )
  
let container_add_on (ls : Lua.state) : int =
  let argc = Lua.gettop ls in
  if argc < 2 then (
    dlog 0 "Not enough arguments to \"container_add_on\"";
    0
  ) else (
    let con : container =
      match Lua.touserdata ls 1 with
      | Some `Userdata l -> l
      | Some `Light_userdata l -> l
      | _ -> failwith "Expected container" in
    (
      if Lua.isstring ls 2 then
        let id = Option.get (Lua.tostring ls 2) in
        ignore_tangible (Templates.put_tangible id con On)
      else
        let thing : tangible =
          match Lua.touserdata ls 2 with
          | Some `Userdata l -> l
          | Some `Light_userdata l -> l
          | _ -> failwith "Expected container" in
        con # add thing On
    );
    0
  )

let run_location_script (loc : location) (script : string) =
  let ls = LuaL.newstate () in
  LuaL.openlibs ls;
  Lua.register ls "purge" purge_location;
  Lua.register ls "add_tangible" add_tangible;
  Lua.register ls "container_add_on" container_add_on;
  Lua.pushlightuserdata ls loc;
  Lua.setglobal ls "self";
  if LuaL.dostring ls script then
    dlog 4 "ran script"
  else
    dlog 0 ("Script failed!! script: " ^ script)
