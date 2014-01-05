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
   creature.ml: this file contains the base class for mobs and characters
*)

open Types
open Helpers
open Base
open Tangible
open Debug
open State
open Container

(* a dummy controller that does not depend on the character class. This
   controller is used for instantiation of the character class after which
   a useful controller is set with a the set_controller method of the
   character *)
class dummy_controller =
  object
    inherit iController
    method send_message (msg : mud_string) : unit = ()
  end 

(* a bodypart is the data that describe the bodypart characteristsics of a
   tangible *)
(*class bodypart (bp : bodypart_type) (a_type : bodypart_type)
    (r_types : bodypart_type list) (t : iTangible) =
  object (self)

    inherit iBodypart

    val mutable receive_list : (bodypart_type * bodypart option) list =
      List.map (function x -> (x, None)) r_types
    val mutable attached_to = None
    val my_type = bp
    val attach_type = a_type
    val my_tangible = t

    method get_type = my_type

    method get_tangible = my_tangible

    method receive (bp : iBodypart) =
      let bt = bp#get_type in
      let rec receive_helper = function
          (t, c)::xs -> if t = bt && c = None then (t, Some bp)::xs
            else (t, c)::(receive_helper xs)
        | []         -> raise (No_attachment bp) in
      receive_list <- receive_helper receive_list

    method attach_to (bp : iBodypart) =
      attached_to <- Some bp

    method get_parts : (bodypart_type * iBodypart) list =
      (my_type, (self : #iBodypart :> iBodypart))::(List.flatten (List.map
            (function (_, Some p) -> p#get_parts | (_, None) -> [])
	    receive_list))
  end*)

let rec get_parts (b : body_part) : body_part list =
  b::(List.flatten (List.map (fun (_, p) -> get_parts p) b.receive_list))

(* a mob class *)
class virtual creature (i : int) (name : string) (b : body_part) =
  object (self)

    inherit iCreature
    inherit tangible i [] name name name []

    val body = b
    val mutable ctrl = new dummy_controller

    (* called to set some controller to be in control of this character *)
    method set_controller (c : iController) =
      ctrl <- c;
      ctrl#send_message (MudStringMeta (MetaInit, MudStringNone))

    (* called to pick up an object *)
    method take (thing : iTangible) =
      if thing == (self : #iCreature :> iTangible) then
        raise (Command_error "You can't pick up yourself.");
      if not (thing#can_be_gotten (self : #iCreature :> iCreature)) then
        raise (Command_error "You can't pick up that object.");
      let hands = map_some (function p ->
            match p.kind with Hand _ -> Some p | _ -> None) (get_parts body) in
      if List.exists (function h -> h.thing#contains In thing) hands
        then raise (Command_error "You are already holding that.");
      let rec take_one_handed = function
          h::hs -> (try thing#move_to [((h.thing :> iContainer), In)]
            with Cannot_add _ -> take_one_handed hs)
        | [] -> raise (No_space_for thing) in
      take_one_handed hands;
      (self#get_location)#relay_message (MudStringCondition
          ((self: #iCreature :> iCreature), MudStringList (SeparatorNone,
            [MudString "You pick up the "; MudStringName thing]),
          MudStringList (SeparatorNone,
            [MudStringName (self: #iCreature :> iTangible);
            MudString " picked up the "; MudStringName thing])))

    method drop (thing : iTangible) =
      let hands = map_some (function b ->
          match b.kind with
            | Hand _ -> if b.thing#contains In thing
	        then Some b.thing
	        else None
	    | _      -> None) (get_parts body) in
      if hands = []
        then raise (Command_error "You aren't holding that.");
      List.iter (function h -> thing#remove_from (h :> iContainer)) hands;
      (self#get_location)#relay_message (MudStringCondition
          ((self: #iCreature :> iCreature), MudStringList (SeparatorNone,
            [MudString "You drop the "; MudStringName thing]), MudStringList
              (SeparatorNone, [MudStringName (self: #iCreature :> iTangible);
              MudString " dropped the "; MudStringName thing])))

    method add (p : preposition) (thing : iTangible) =
      (* FIXME: this part needs to be totally redone *)
      (*match thing#as_bodypart with
          Some x -> ()
        | None   -> raise (Cannot_add thing)*) ()

    (* FIXME: kill this function, make it exist outside the class or merge the
       functionality with find *)
    method look_for (desc : object_desc) =
      let rec search_containers containers count =
        match containers with
          | [] -> raise (Object_not_found (desc, count))
          | l::ls -> try find (self : #iCreature :> iCreature)
	      l Anywhere desc
            with Object_not_found (_, num) -> search_containers ls num in
      try
        find (self : #iCreature :> iCreature) (self : #iContainer :> iContainer)
	  Anywhere desc
      with
          Object_not_found (_, num) -> search_containers self#get_containers num

    method get_inventory (looker : iCreature) : inventory =
      List.map (function p ->
          (p.kind, Anywhere, p.thing#get_contents looker Anywhere))
        (get_parts body)

    method is_visible looker = looker != (self : #iCreature :> iCreature)

    method as_creature = Some (self : #iCreature :> iCreature)
  end
