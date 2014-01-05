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

(* tangible.ml: this file contains the tangible class. a tangible is any
   object in the mud than can be manipulated in some way.
*)

open Types
open Helpers
open Container
open Base
open State

(* base physical object *)
class tangible (i : int) (a : string list) (n : string) (sd : string)
    (ld : string) (ps : preposition list) =
  object (self)

    inherit iTangible
    inherit container

    val id = if i < 0 then tangibles#get_id else i
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
      let old_len = List.length containers in
      containers <- List.filter (function c -> c != con) containers;
      if old_len = List.length containers then
        raise (Cannot_remove (self : #iTangible :> iTangible));
      con#remove (self : #iTangible :> iTangible);
      if containers = [] then self#add_to On (loc :> iContainer)

    method add_to (prep : preposition) (con : iContainer) : unit =
      con#add prep (self : #iTangible :> iTangible);
      containers <- con::containers

    method move_to (dest_cs : (iContainer * preposition) list) : unit =
      let this = (self : #iTangible :> iTangible) in
      (** First check that we can do this **)
      (* if can't remove from containers abort *)
      if List.exists (fun a -> not (a#can_remove this)) containers then
        raise (Cannot_remove (self : #iTangible :> iTangible))
      (* if can't add to the new containers abort *)
      else if List.exists (fun (c, p) -> not (c#can_add p this)) dest_cs then
        raise (Cannot_add (self : #iTangible :> iTangible))
      else begin
        (** then do it **)
        (* remove from old containers *)
        List.iter (fun a -> a#remove this) containers;
        (* add to new containers *)
        List.iter (fun (c, p) -> c#add p this) dest_cs;
        (* make the new containers the current containers *)
        let (dest_cons, _) = List.split dest_cs in
        containers <- dest_cons
      end

    method matches_description sadjs sname =
      if not (starts_with name sname) then false
      else not (List.exists (function sadj -> List.exists
            (function adj -> not (starts_with adj sadj)) adjs) sadjs)

    method can_be_found looker = true

    method can_be_gotten looker = true

    method is_visible looker = true

    method get_name = name

    method get_short_desc looker = MudString short_desc

    method get_long_desc looker = MudString long_desc

    method get_containers = containers

    initializer
      State.tangibles#add id (self : #iTangible :> iTangible)
  end
