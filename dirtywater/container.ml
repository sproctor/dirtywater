(*pp camlp4o *)
(*
 Copyright 2014, 2003 Sean Proctor, Mike MacHenry

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
   container.ml: this file contains the basic container class. it provides
   the basic implementation of a container. any object that needs to store
   things in a general way should be derived from this or should implement
   iContainer
*)

open Types
open Helpers
open Base
open Debug

(* an object that can contain things *)
class virtual container =
  object (self)

    inherit iContainer

    val mutable contents : iTangible list = []
    val mutable position_list : (position * iTangible) list = []

    method can_add (thing : iTangible) : bool =
      not (self#contains thing)

    method add (thing : iTangible) : unit =
      if self#contains thing then
        raise (Cannot_add thing)
      else contents <- thing::contents

    method can_remove (thing : iTangible) : bool =
      self#contains thing

    method remove (thing : iTangible) : unit =
      let len = List.length contents in
      contents <- List.filter (function t -> t <> thing) contents;
      if len - 1 <> List.length contents then raise (Cannot_remove thing)

    method contains (thing : iTangible) : bool =
      List.mem thing self#get_contents

    method get_contents : iTangible list =
       contents

    method get_contents_recursive : iTangible list =
       let c = self#get_contents in
       c @ (List.flatten (List.map (fun t -> t#get_contents_recursive) c))

    method view_contents (looker : iCreature) : iTangible list =
      List.filter (fun t -> t#is_visible looker) (self#get_contents)

    method view_contents_recursive (looker : iCreature) : iTangible list =
      let c = self#view_contents looker in
      (c @ (List.flatten (List.map (fun t -> t#view_contents_recursive looker)
            c)))

  end

let filter_contents (adjs : string list) (name : string)
    (l : iTangible list) : iTangible list =
    List.filter (fun n -> n#matches_description adjs name) l

let rec find_position (where : position) (looker : iCreature)
    (lookee : iContainer) (desc : object_desc) : iTangible =
  let items = 
let rec find (looker : iCreature) (lookee : iContainer) (desc : object_desc)
    : iTangible =
  let items = lookee#view_contents_recursive looker in
  dlog 4 ("Searching for " ^ object_desc_to_string desc);
  match desc with
    | ObjectDesc (od, p, (n, adjs, name)) ->
        begin
          let matched_items = filter_contents adjs name items in
          try
            let item = List.nth matched_items ((Option.default 1 n) - 1) in
            dlog 4 "found the first item";
            begin 
              match p with
              | Prep_in -> find_position In looker item od
              | Prep_on -> find_position On looker item od
                (* try all containment methods with "from" *)
              | Prep_from -> find looker item od
              | Prep_under -> raise (Bad_command "Preposition \"under\" is not yet supported.")
              | Prep_behind -> raise (Bad_command "Preposition \"behind\" is not yet supported.")
            end
          with Failure "nth" -> raise (dlog 4 "object not found"; Object_not_found (desc, List.length matched_items))
        end
    | ObjectDescBase (n, adjs, name) ->
        let matched_items = filter_contents adjs name items in
        begin
          try
            let item = List.nth matched_items ((Option.default 1 n) - 1) in
            dlog 4 "found the item";
            item
          with Failure "nth" ->
            dlog 4 "object not found";
            raise (Object_not_found (desc, List.length matched_items))
        end
