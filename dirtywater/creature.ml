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
class bodypart (t : bodypart_type) (r_parts : iBodypart list) =
  let (adjs, name) = bodypart_to_desc t in
  let desc = String.concat " " (adjs@[name]) in
  object (self)

    inherit iBodypart
    inherit tangible (-1) adjs name desc desc as super

    val mutable receive_list : iBodypart list = r_parts
    val mutable attached_to : iBodypart option = None
    val my_type : bodypart_type = t

    method get_type : bodypart_type = my_type

    method receive (bp : iBodypart) : unit =
      receive_list <- bp::receive_list

    method attach_to (bp : iBodypart) : unit =
      attached_to <- Some bp

    method get_parts : iBodypart list =
      (self : #iBodypart :> iBodypart)::(List.flatten (List.map
            (function p -> p#get_parts) receive_list))

    initializer
      List.iter (function bp -> bp#attach_to (self : #iBodypart :> iBodypart))
          receive_list
  end

(* a mob class *)
class virtual creature (i : int) (name : string) (b : bodypart) =
  object (self)

    inherit iCreature
    inherit tangible i [] name name name

    val body = b
    val mutable ctrl = new dummy_controller

    (* called to set some controller to be in control of this character *)
    method set_controller (c : iController) =
      ctrl <- c;
      ctrl#send_message (MudStringMeta (MetaInit, MudStringNone))

    (* called to pick up an object *)
    method take (thing : iTangible) =
      dlog 4 "called take";
      if thing == (self : #iCreature :> iTangible) then
        raise (Command_error "You can't pick up yourself.");
      if not (thing#can_be_gotten (self : #iCreature :> iCreature)) then
        raise (Command_error "You can't pick up that object.");
      let hands = List.filter (function p ->
            match p#get_type with Hand _ -> true | _ -> false) body#get_parts in
      dlog 0 "got hands";
      if List.exists (function h -> (h#get_container In)#contains thing) hands
        then raise (Command_error "You are already holding that.");
      dlog 0 "checked hands";
      let rec take_one_handed = function
          h::hs -> (try thing#move_to (h#get_container In)
            with Cannot_add _ -> take_one_handed hs)
        | [] -> raise (No_space_for thing) in
      dlog 4 ("creature is picking up " ^ (thing#to_string));
      take_one_handed hands;
      dlog 4 "and got it.";
      (self#get_location)#relay_message (MudStringCondition
          ((self: #iCreature :> iCreature), MudStringList (SeparatorNone,
            [MudString "You pick up the "; MudStringName thing]),
          MudStringList (SeparatorNone,
            [MudStringName (self: #iCreature :> iTangible);
            MudString " picked up the "; MudStringName thing])))

    method drop (thing : iTangible) =
      (* FIXME: if two hands share a container to hold something,
           then this is won't work *)
      let get_holder b =
        match b#get_type with
        | Hand _ -> let c = b#get_container In in
            if c#contains thing then Some c else None
	| _ -> None
      in
      let hand_containers = map_some get_holder (body#get_parts) in
      if hand_containers = []
        then raise (Command_error "You aren't holding that.");
      thing#move_to ((self#get_location) :> iContainer);
      (self#get_location)#relay_message (MudStringCondition
          ((self: #iCreature :> iCreature), MudStringList (SeparatorNone,
            [MudString "You drop the "; MudStringName thing]), MudStringList
              (SeparatorNone, [MudStringName (self: #iCreature :> iTangible);
              MudString " dropped the "; MudStringName thing])))

    (* FIXME: kill this function, make it exist outside the class or merge the
       functionality with find *)
    method look_for (desc : object_desc) =
      try
        find (self : #iCreature :> iCreature) (self#get_container In)
	  desc
      with
          (* FIXME: this searches self twice and messes up the ordinal *)
          Object_not_found (_, num) -> find (self : #iCreature :> iCreature)
              (self#get_parent) desc

    method get_inventory (looker : iCreature) : inventory =
      List.flatten (
        List.map (fun p ->
           map_some
             (fun con ->
               try
                 let container = p#get_container con in
                 Some (p#get_type, con, container#view_contents looker)
               with Not_found -> None (* container not found *)
             ) [In; On]
         )
       (body#get_parts))

    method is_visible looker = looker != (self : #iCreature :> iCreature)

    method as_creature = Some (self : #iCreature :> iCreature)

    initializer
      let c = new container (Some (self : #iMud_object :> iMud_object)) in
      containers <- [In, c];
      body#set_parent (Some (self#get_container In));
      c#add (body :> iTangible)
  end

class hand_container (p : iMud_object) =
  object (self)
    inherit iContainer

    val parent = p
    val mutable contents : iTangible option = None

    method to_string =
      "hand container for: " ^ (parent#to_string)

    method can_add (thing : iTangible) : bool =
      Option.is_none contents

    method add (thing : iTangible) : unit =
      if Option.is_some contents then
        raise (Cannot_add thing)
      else contents <- Some thing

    method can_remove (thing : iTangible) : bool =
      contents = Some thing

    method remove (thing : iTangible) : unit =
      if contents <> Some thing then
        raise (Cannot_remove thing)
      else contents <- None

    method contains (thing : iTangible) : bool =
      contents = Some thing

    method get_contents : iTangible list =
      match contents with
      | Some t -> [t]
      | None -> []

    method view_contents (looker : iCreature) : iTangible list =
      List.filter (fun t -> t#is_visible looker) (self#get_contents)

    method get_location : iLocation =
      parent#get_location
  end
