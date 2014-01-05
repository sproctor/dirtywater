(*
 Copyright 2004, 2003 Sean Proctor, Mike MacHenry

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
   load_template.ml: this file contains the functions to load the templates
     to create tangibles
*)

open Pxp_yacc
open Pxp_document

open Helpers
open Debug
open Types
open Base
open Tangible
open Load
open Race

class default_race_ext =
  object

    inherit [default_race_ext] generic_default_ext

    method create_race : iRace = assert false

    method set_body_part (p : body_part) : unit = ()
    method set_body (r : iRace) : unit = ()
  end

class race_ext =
  object (self)

    inherit default_race_ext

    method create_race : iRace =
      let n = get_opt node in
      let children = n#sub_nodes in
      let id = int_of_string (n#required_string_attribute "id") in
      let name = n#required_string_attribute "name" in
      let r = new race id name in
      List.iter (function n -> n#extension#set_body r) children;
      r
  end

class body_part_ext =
  object (self)

    inherit default_race_ext

    method private get_part (p : body_part option) : body_part =
      let n = get_opt node in
      let children = n#sub_nodes in
      let t = n#required_string_attribute "type" in
      let s = n#optional_string_attribute "side" in
      let adjs = match s with
          | Some a -> [a]
          | None   -> [] in
      let kind = body_part_kind_of_string t s in
      let tan = new tangible (-1) adjs t t t [] in
      let bp =
          { kind = kind; attached_to = p; receive_list = []; thing = tan;} in
      (match p with
          | Some parent ->
              parent.receive_list <- (kind, bp)::parent.receive_list
          | None -> ());
      List.iter (function n -> n#extension#set_body_part bp) children;
      bp

    method set_body_part p =
      ignore (self#get_part (Some p))

    method set_body r =
      r#set_body (self#get_part None)
  end

let race_spec = make_spec_from_alist
    ~data_exemplar:             (new data_impl (new default_race_ext))
    ~default_element_exemplar:  (new element_impl (new default_race_ext))
    ~element_alist:             [
        "race",                 new element_impl (new race_ext);
        "body-part",            new element_impl (new body_part_ext);
      ]
    ()

let load_races () =
  let create_race (str: string) =
    dlog 0 ("filename: " ^ str ^ "\n");
    let d = parse_document_entity default_config (from_file str) race_spec
      in
    (d, d#root#extension#create_race) in
  ignore (load_generic "data/races" create_race)
