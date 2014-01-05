(*pp camlp4o *)
(*
 Copyright 2003 Sean Proctor, Mike MacHenry

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
    inherit mud_object

    val mutable contents : (preposition * iTangible) list = []

    method can_add (prep : preposition) (thing : iTangible) =
      not (List.exists ((==) thing) (self#get prep))

    method add (prep : preposition) (thing : iTangible) : unit =
      if List.exists ((==) thing) (self#get prep) then
        raise (Failure "container#add")
      else contents <- (prep, thing)::contents

    method can_remove (thing : iTangible) = true

    method remove (thing : iTangible) : unit =
      let len = List.length contents in
      contents <- List.filter (function (_, t) -> t != thing) contents;
      if len - 1 <> List.length contents then raise (Failure "container#remove")

    method get (prep : preposition) : iTangible list =
      List.map (function (_, t) -> t)
        (List.filter self#preposition_matches contents)

    method contains (prep : preposition) (thing : iTangible) : bool =
      List.exists ((==) thing) (self#get prep)

    method private preposition_matches
      ((prep : preposition), (thing : iTangible)) : bool =
      try
        let (p, _) = List.find (function (_, t) -> t == thing) contents in
        prep = p
      with Not_found -> false

    method get_contents (looker : iCreature) (prep : preposition)
        : iTangible list =
      map_some (function (p, o) -> if (prep = Anywhere || p = prep)
            then Some o else None) contents
  end

let rec get_full_contents (looker : iCreature) (prep : preposition)
    (lookee : iContainer) : iTangible Stream.t =
  let contents = lookee#get_contents looker prep in
  [< (list_to_stream contents);
    map_to_stream (function t -> get_full_contents looker prep
          (t :> iContainer)) contents >]

let rec filter_contents (adjs : string list) (name : string)
    : iTangible Stream.t -> iTangible Stream.t = parser
  [< 'n; s >] -> if n#matches_description adjs name
      then [< 'n; filter_contents adjs name s >]
      else [< filter_contents adjs name s >]

let rec find (looker : iCreature) (lookee : iContainer) (prep : preposition)
    (desc : object_desc) : iTangible =
  let items = get_full_contents looker prep lookee in
  match desc with
    | ObjectDesc (od, p, (n, adjs, name)) ->
        let item = stream_nth (get_opt_default n 1)
            (filter_contents adjs name items) in
        find looker (item :> iContainer) p od
    | ObjectDescRelative (od, p) ->
        find looker lookee prep od
    | ObjectDescBase (n, adjs, name) ->
        stream_nth (get_opt_default n 1) (filter_contents adjs name items)
