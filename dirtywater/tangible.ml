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

open Types
open Helpers
open Container
open Base

(* base physical object that physical objects should be derived from *)
class tangible (a : string list) (n : string) (sd : string) (ld : string)
    (ps : preposition list) =
  object (self)
    inherit iTangible
    inherit container
    val name = n
    val adjs = a
    val short_desc = sd
    val long_desc = ld
    val mutable containers : iContainer list = []
    method get_location : iLocation =
      match containers with
          x::xs -> x#get_location
        | [] -> raise (Failure "Got location from object not in the world.")
    method remove_from (con : iContainer) : unit =
      let loc = self#get_location in
      let len = List.length containers in
      containers <- List.filter (function c -> c != con) containers;
      if len = List.length containers then
        raise (Cannot_remove (self : #iTangible :> iTangible));
      con#remove (self : #iTangible :> iTangible);
      if containers = [] then self#add_to On (loc :> iContainer)
    method add_to (prep : preposition) (con : iContainer) : unit =
      con#add prep (self : #iTangible :> iTangible);
      containers <- con::containers
    method move_to (cs : (iContainer * preposition) list) : unit =
      let (cons, _) = List.split cs in
      let remove_from (a : iContainer) =
        a#remove (self : #iTangible :> iTangible) in
      let add_to (c, p) =
        c#add p (self : #iTangible :> iTangible) in
      let cannot_remove_from (a : iContainer) =
        not (a#can_remove (self : #iTangible :> iTangible)) in
      let cannot_add_to (c, p) =
        not (c#can_add p (self : #iTangible :> iTangible)) in
      if List.exists cannot_remove_from containers then
        raise (Cannot_remove (self : #iTangible :> iTangible))
      else if List.exists cannot_add_to cs then
        raise (Cannot_add (self : #iTangible :> iTangible))
      else begin
        List.iter remove_from containers;
        List.iter add_to cs;
        containers <- cons
      end
    method matches_description sadjs sname =
      if not (starts_with name sname) then false
      else not (List.exists (function sadj -> List.exists
            (function adj -> not (starts_with adj sadj)) adjs) sadjs)
    method can_be_found looker = true
    method can_be_gotten looker = true
    method is_visible looker = true
    method get_name : string = name
    method get_short_desc looker = short_desc
    method get_long_desc looker = long_desc
    method as_bodypart = None
    method get_containers = containers
    method send_messages msgs = ()
  end

let make_tangible_in_room ?(adjs = []) ~name ?short_desc ?long_desc room =
  let sd = (match short_desc with Some x -> x
    | None -> String.concat " " (adjs@[name])) in
  let ld = (match long_desc with Some x -> x | None -> sd) in
  let thing = new tangible adjs name sd ld [] in
  thing#move_to [((room :> iContainer), Anywhere)];
  thing
