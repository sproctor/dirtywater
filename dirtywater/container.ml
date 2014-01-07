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

    method can_add (thing : iTangible) : bool =
      not (self#contains thing)

    method add (thing : iTangible) : unit =
      if self#contains thing then
        raise (Failure "container#add")
      else contents <- thing::contents

    method can_remove (thing : iTangible) =
      List.exists (function (_, t) -> t = thing) contents

    method remove (thing : iTangible) : unit =
      let len = List.length contents in
      contents <- List.filter (function (_, t) -> t != thing) contents;
      if len - 1 <> List.length contents then raise (Failure "container#remove")

    method contains (thing : iTangible) : bool =
      List.exists ((=) thing) contents

    method get_contents : iTangible list =
       contents

    method view_contents (looker : iCreature) : iTangible list =
      List.filter (function t -> t#is_visible looker) (self#get_contents)
  end

let rec get_full_contents (looker : iCreature) (con : containment option)
    (lookee : iContainer) : iTangible Stream.t =
  let contents = lookee#view_contents looker con in
  [< Stream.of_list contents;
    map_to_stream (function t -> get_full_contents looker con
          (t :> iContainer)) contents >]

let rec filter_contents (adjs : string list) (name : string)
    : iTangible Stream.t -> iTangible Stream.t = parser
    [< 'n; s >] -> dlog 4 ("filtering " ^ name);
      if n#matches_description adjs name
      then [< 'n; filter_contents adjs name s >]
      else [< filter_contents adjs name s >]
  | [< >] -> [< >]

let rec find (looker : iCreature) (lookee : iContainer) (con : containment option)
    (desc : object_desc) : iTangible =
  let items = get_full_contents looker con lookee in
  dlog 4 ("Searching for " ^ object_desc_to_string desc);
  match desc with
    | ObjectDesc (od, p, (n, adjs, name)) ->
        let istream = (filter_contents adjs name items) in
        (match stream_nth (get_opt_default n 1) istream with
           | Some item -> dlog 4 "found the first item"; find looker (item :> iContainer) (preposition_to_containment_option p) od
           | None -> raise (dlog 4 "object not found"; Object_not_found (desc, Stream.count istream)))
    | ObjectDescRelative (od, p) ->
        find looker lookee con od
    | ObjectDescBase (n, adjs, name) ->
        let istream = (filter_contents adjs name items) in
        (match stream_nth (get_opt_default n 1) istream with
           | Some item -> dlog 4 "found the item"; item
           | None -> raise (dlog 4 "object not found"; Object_not_found (desc, Stream.count istream)))
