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
   container
*)

open Types
open Helpers
open Base
open Debug

(* an object that can contain things *)
class virtual simple_container =
  object (self)

    inherit container

    val mutable contents : (position * tangible) list = []

    method can_add (actor : creature) (thing : tangible) (where : position)
        : bool =
      not (List.exists (fun (w, t) -> w = where && t == thing) contents)

    (* This function is not safe. The caller should check that they're allowed
       to add the thing before they call it *)
    method add (thing : tangible) (where : position) : unit =
      dlog 4 ("adding " ^ thing#to_string ^ " to " ^ self#to_string);
      (* We might be adding a thing that's already here to a new position *)
      (*if not (List.mem thing contents) *)
      contents <- (where, thing)::contents

    method can_remove (actor : creature) (thing : tangible) : bool =
      self#contains thing

    method remove (thing : tangible) : unit =
      let do_remove where =
        let rec do_helper contents_list =
          match contents_list with
          | (_, t)::rest when t == thing -> do_helper rest
          | (Under t, other)::rest when t == thing ->
              (where, other)::do_helper rest
          | (Behind t, other)::rest when t == thing ->
              (where, other)::do_helper rest
          | t::rest -> t::do_helper rest
          | [] -> []
      in do_helper contents in
      let rec remove_helper contents_list =
        match contents_list with
        | (w, t)::rest -> if t == thing then contents <- do_remove w
            else remove_helper rest
        | [] -> raise (Cannot_remove thing)
      in remove_helper contents

    method get_contents (where : position option) : tangible list =
      match where with
      | Some pos ->
          let compare_pos (p, t) =
            if p = pos then Some t
            else None
          in
          map_some compare_pos contents
      | None -> List.map (fun (_, t) -> t) contents

    method view_contents (looker : creature) (where : position option)
        : tangible list =
      List.filter (fun t -> t#is_visible looker) (self#get_contents where)

    method private contains (thing : tangible) : bool =
      List.exists (fun (_, t) -> t == thing) contents

  end

class virtual noncontainer =
  object
    inherit container

    method can_add looker thing where = false
    method add thing where = raise (Cannot_add thing)
    method can_remove actor thing = false
    method remove thing = raise (Cannot_remove thing)
    method get_contents where = []
    method view_contents looker where = []
  end

let rec view_contents_recursive (lookee : container) (where : position option)
    (looker : creature) : tangible list =
  let contents = lookee#view_contents looker where in
  let recursive_helper (thing : tangible) : tangible list =
    view_contents_recursive (thing :> container) None looker in
  contents @ (List.flatten (List.map recursive_helper contents))

let filter_contents (adjs : string list) (name : string)
    (l : tangible list) : tangible list =
    List.filter (fun n -> n#matches_description adjs name) l

let rec find (looker : creature) (lookee : container) (p : position option)
    (desc : object_desc) : tangible =
  let items = view_contents_recursive lookee p looker in
  match desc with
    | ObjectDesc (od, p, (n, adjs, name)) ->
        begin
          let matched_items = filter_contents adjs name items in
          try
            let item = List.nth matched_items ((Option.default 1 n) - 1) in
            dlog 4 "found the first item";
            begin 
              match p with
              | Prep_in -> find looker (item :> container) (Some In) od
              | Prep_on -> find looker (item :> container) (Some On) od
                (* try all containment methods with "from" *)
              | Prep_from -> find looker (item :> container) None od
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
