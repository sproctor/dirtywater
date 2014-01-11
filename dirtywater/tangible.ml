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
    (ld : string) =
  object (self)

    inherit iTangible
    inherit container

    val id = if i < 0 then tangibles#get_id else i
    val name = n
    val adjs = a
    val short_desc = sd
    val long_desc = ld
    val mutable parent : iContainer option = None

    method get_location : iLocation = (self#get_parent)#get_location

    method get_parent : iContainer =
      match parent with
      | Some p -> p
      | None -> raise (Failure "Trying to access an object not in the world.")

    method set_parent (p : iContainer option) : unit =
      parent <- p

    method move_to (dest : iContainer) : unit =
      let this = (self : #iTangible :> iTangible) in
      (** First check that we can do this **)
      (* if can't remove from parent abort *)
      if not ((self#get_parent)#can_remove this) then
        raise (Cannot_remove (self : #iTangible :> iTangible))
      (* if can't add to the new parent abort *)
      else if not (dest#can_add this)
        then raise (Cannot_add (self : #iTangible :> iTangible))
      else begin
        (** then do it **)
        (* remove from old parent *)
        (self#get_parent)#remove this;
        (* add to new parent *)
        dest#add this;
        (* make the new parent the current parent *)
        parent <- Some dest
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

    method as_creature = None

    method to_string : string =
      "tangible" ^ (string_of_int id) ^ ": " ^ short_desc

    initializer
      State.tangibles#add id (self : #iTangible :> iTangible)
  end
