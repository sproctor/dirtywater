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
class container (p : iMud_object option) =
  object (self)

    inherit iContainer

    val parent = p
    val mutable contents : iTangible list = []

    method to_string =
      let pstring =
        match parent with
        | Some p -> p#to_string
        | None -> "nothing"
      in
      "container for: " ^ pstring

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

    method get_contents_recursive (looker : iCreature) : iTangible list =
      let c = List.filter (fun t -> t#is_visible looker) (self#get_contents) in
      c @ (List.flatten (List.map (fun t -> t#view_contents_recursive looker)
            c))

    method get_location : iLocation =
      match parent with
      | Some p -> p#get_location
      | None -> raise (Failure "Container is not in the world")
  end

let rec view_full_contents_tangible (looker : iCreature) (lookee : iTangible)
    : iTangible Stream.t =
  let contents = lookee#view_contents looker in
  [< Stream.of_list contents;
    map_to_stream (function t -> view_full_contents_tangible looker t)
        contents >]

let rec filter_contents (adjs : string list) (name : string)
    : iTangible list -> iTangible list =
    List.filter (fun 
    [< 'n; s >] -> dlog 4 ("filtering " ^ name);
      if n#matches_description adjs name
      then [< 'n; filter_contents adjs name s >]
      else [< filter_contents adjs name s >]
  | [< >] -> [< >]

let rec find (looker : iCreature) (lookee : iContainer) (desc : object_desc)
    : iTangible =
  let items = view_full_contents_container looker lookee in
  dlog 4 ("Searching for " ^ object_desc_to_string desc);
  match desc with
    | ObjectDesc (od, p, (n, adjs, name)) ->
        let istream = (filter_contents adjs name items) in
        begin
          match stream_nth (Option.default 1 n) istream with
          | Some item -> dlog 4 "found the first item";
              begin 
                match p with
                | Prep_in -> find looker (item#get_container In) od
                | Prep_on -> find looker (item#get_container On) od
                  (* try all containment methods with "from" *)
                | Prep_from ->
                    begin
                      try find looker (item#get_container On) od
                      with
                      | Object_not_found _ -> find looker
                          (item#get_container In) od
                    end
                | Prep_under -> raise (Bad_command "Preposition \"under\" is not yet supported.")
                | Prep_behind -> raise (Bad_command "Preposition \"behind\" is not yet supported.")
              end
          | None -> raise (dlog 4 "object not found"; Object_not_found (desc, Stream.count istream))
        end
    | ObjectDescBase (n, adjs, name) ->
        let istream = (filter_contents adjs name items) in
        begin
          match stream_nth (Option.default 1 n) istream with
          | Some item -> dlog 4 "found the item"; item
          | None -> raise (dlog 4 "object not found"; Object_not_found (desc, Stream.count istream))
        end
