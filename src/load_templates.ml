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

open Debug
open Helpers
open Load
open Template

let rec parse_template (node: YamlNode.t) =
  match node with
  | YamlNode.SEQUENCE (_, nodes) -> List.iter parse_template nodes
  | YamlNode.MAPPING (_, map) ->
      let parse_item = function
        | (YamlNode.SCALAR (_, "item"), YamlNode.MAPPING (_, values)) -> begin
            print_endline "found item";
            let id = ref None in
            let noun = ref None in
            let sdesc = ref None in
            let ldesc = ref None in
            let parse_value = function
              | (YamlNode.SCALAR (_, key), YamlNode.SCALAR (_, value)) -> begin
                  match key with
                  | "id" -> id := Some value
                  | "noun" -> noun := Some value
                  | "sdesc" -> sdesc := Some value
                  | "ldesc" -> ldesc := Some value
                  | _ -> ()
                end
              | _ -> () in
            List.iter parse_value values;
            try
              let template = new simple_template [] (Option.get !noun) (Option.get !sdesc) (Option.get !ldesc) in
              Template_collection.templates # add (Option.get !id) template;
              print_endline "added item"
            with Option.No_value -> ()
          end
        | _ -> () in
      List.iter parse_item map
  | _ -> ()

let load_templates () =
  let create_template (filename: string) =
    dlog 0 ("loading filename: " ^ filename ^ "\n");
    let p = YamlParser.make () in
    try
      let root = YamlParser.parse_string p (load_file filename) in
      parse_template root
    with YamlParser.Error (msg) ->
      prerr_endline msg
  in
  load_generic "data/tangibles" create_template
