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
open Base
open Tangible
open Debug

(* an dummy controller that does not depend on the character class. This
   controller is used for instantiation of the character class after which
   a useful controller is set with a the set_controller method of the
   character *)
class dummy_controller =
  object
    inherit iController
    method send_messages (msgs : message list) : unit = ()
  end 

class bodypart (bp : bodypart_type) (a_type : bodypart_type)
    (r_types : bodypart_type list) =
  let (adjs, name) = bodypart_to_desc bp in
  let desc = String.concat " " (adjs@[name]) in
  object (self)
    inherit tangible adjs name desc desc [In; On] as super
    inherit iBodypart
    val mutable receive_list : (bodypart_type * bodypart option) list =
      List.map (function x -> (x, None)) r_types
    val mutable attached_to = None
    val my_type = bp
    val attach_type = a_type
    method private get_received =
      List.map (function (_, Some bp) -> bp
          | (_, None) -> raise (Failure "bodypart#get_hands"))
        (List.filter (fun (_, x) -> x <> None) receive_list)
    method get_type = my_type
    method receive (bp : iBodypart) =
      let bt = bp#get_type in
      let rec receive_helper = function
          (t, c)::xs -> if t = bt && c = None then (t, Some bp)::xs
            else (t, c)::(receive_helper xs)
        | []         -> raise (No_attachment bp) in
      receive_list <- receive_helper receive_list
    method attach_to (bp : iBodypart) =
      attached_to <- Some bp
    method get_hands =
      List.flatten (List.map (function bp -> bp#get_hands) self#get_received)
    method get_inventory : inventory =
      (List.filter (function (_, _, x) -> x <> [])
        (List.map (function p -> (my_type, p, self#get p)) [On; In]))
        @(List.flatten (List.map (function bp -> bp#get_inventory)
            self#get_received))
    method as_bodypart = Some (self : #iBodypart :> iBodypart)
    method find looker desc count =
      let rec find_helper count part_list =
        match part_list with
          | [] -> raise (Object_not_found (desc, count))
          | bp::bps -> try bp#find looker desc count
              with Object_not_found (desc, num) -> find_helper num bps in
      try super#find looker desc count
      with Object_not_found (_, num) -> find_helper num self#get_received
  end

class hand (s : side) =
  object (self)
    inherit bodypart (Hand s) (Arm s) [] as body
    method add (p : preposition) (o : iTangible) =
      if p = In && self#get In = [] then body#add p o
      else raise (Failure "hand#add")
    method can_add p thing =
      if p = In && self#get In <> [] then (
        dlog 0 ("Cannot add to hand, # items is " ^ string_of_int
          (List.length (self#get In)));
        false
      ) else body#can_add p thing
    method get_hands = [(self : #iBodypart :> iBodypart)]
  end

(* a mob class *)
class virtual creature (adjs : string list) (name : string) (b : iBodypart) =
  object (self)
    inherit iCreature
    inherit tangible adjs name name name []
    val body = b
    val mutable ctrl = new dummy_controller
    (* called to some control to be in control of this character *)
    method set_controller (c : iController) =
      ctrl <- c;
      ctrl#send_messages [(Msg_init, "")]
    (* called to pick up an object *)
    method take (o : iTangible) =
      if o == (self : #iCreature :> iTangible) then
        raise (Command_error "You can't pick up yourself.");
      if not (o#can_be_gotten (self : #iCreature :> iCreature)) then
        raise (Command_error "You can't pick up that object.");
      let hands = body#get_hands in
      if List.exists (function h -> (h :> iContainer)#contains In o) hands then
        raise (Command_error "You are already holding that.");
      let rec take_one_handed = function
          h::hs -> (try o#move_to [((h :> iContainer), In)]
            with Cannot_add _ -> take_one_handed hs)
        | [] -> raise (No_space_for o) in
      take_one_handed hands
    method drop (thing : iTangible) =
      let hands = List.filter (function h -> (List.exists
        ((==) (h :> iContainer)) thing#get_containers)) body#get_hands in
      if hands = [] then raise (Command_error "You aren't holding that.");
      List.iter (function h -> thing#remove_from (h :> iContainer)) hands
    method add (p : preposition) (thing : iTangible) =
      match thing#as_bodypart with
          Some x -> ()
        | None   -> raise (Cannot_add thing)
    method look_for (desc : object_desc) =
      let rec search_containers containers count =
        match containers with
          | [] -> raise (Object_not_found (desc, count))
          | l::ls -> try l#find (self : #iCreature :> iCreature) desc count
             with Object_not_found (_, num) -> search_containers ls num in
      try self#find (self : #iCreature :> iCreature) desc 0
      with Object_not_found (_, num) -> search_containers self#get_containers
          num
    method find looker desc count = body#find looker desc count
    method get_inventory = body#get_inventory
    method get_body = body
    method is_visible looker = looker != (self : #iCreature :> iCreature)
  end
