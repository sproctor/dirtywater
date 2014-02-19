(*
 Copyright 2014 Sean Proctor, Mike MacHenry

 This file is part of Dirty Water.

 Dirty Water is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 Dirty Water is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Dirty Water; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(*
   load.ml: this file contains the generic function to load the data files
*)

open Helpers
open Load
open Template
open Base

let rec parse_locations (node : YamlNode.t) (dir : string) =
  match node with
  | YamlNode.SEQUENCE (_, nodes) -> List.iter (fun x -> parse_locations x dir) nodes
  | YamlNode.MAPPING (_, map) ->
      let parse_location = function
        | (YamlNode.SCALAR (_, "location"), YamlNode.MAPPING (_, values)) -> begin
            print_endline "found location";
            let id = ref None in
            let title = ref None in
            let desc = ref None in
            let init = ref "" in
            let portals = ref [] in
            let parse_value = function
              | (YamlNode.SCALAR (_, key), YamlNode.SCALAR (_, value)) -> begin
                  match key with
                  | "id" -> id := Some (int_of_string value)
                  | "title" -> title := Some value
                  | "desc" -> desc := Some value
                  | "init_file" -> init := load_file (dir ^ "/" ^ value)
                  | _ -> ()
                end
              | (YamlNode.SCALAR (_, "portals"), node) -> begin
                  let rec parse_portal = function
                    | YamlNode.SEQUENCE (_, nodes) -> List.iter parse_portal nodes
                    | YamlNode.MAPPING (_, map) -> begin
                        let dir = ref None in
                        let dest = ref None in
                        let parse_portal_value = function
                          | (YamlNode.SCALAR (_, key), YamlNode.SCALAR (_, value)) -> begin
                              match key with
                              | "dir" -> dir := Some (direction_of_string value)
                              | "dest" -> dest := Some (int_of_string value)
                              | _ -> ()
                            end
                          | _ -> () in
                        List.iter parse_portal_value map;
                        try
                          portals := (Option.get !dir, Option.get !dest) :: !portals
                        with Option.No_value -> ()
                      end
                    | _ -> () in
                  parse_portal node
                end
              | _ -> () in
            List.iter parse_value values;
            try
              let room = Convenience.create_room (Option.get !id) (Option.get !title) (Option.get !desc) !portals in
              room # set_init !init;
              print_endline "added location"
            with Option.No_value -> ()
          end
        | _ -> () in
      List.iter parse_location map
  | _ -> ()

let load_locations () =
  let dir = "data/locations" in
  let create_location (filename: string) =
    dlog 0 ("loading filename: " ^ filename ^ "\n");
    let p = YamlParser.make () in
    try
      let root = YamlParser.parse_string p (load_file filename) in
      parse_locations root dir
    with YamlParser.Error (msg) ->
      prerr_endline msg
  in
  load_generic dir create_location
