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
open Container
open Time
open Events

(* a dummy controller that does not depend on the character class. This
   controller is used for instantiation of the character class after which
   a useful controller is set with a the set_controller method of the
   character *)
class dummy_controller =
  object
    inherit controller
    method send_message (msg : mud_string) : unit = ()
  end 

(* a bodypart is the data that describe the bodypart characteristsics of a
   tangible *)
class bodypart (t : bodypart_type) (c : container) =
  let (adjs, name) = bodypart_to_desc t in
  let desc = String.concat " " (adjs@[name]) in
  object (self)

    inherit simple_tangible adjs name desc desc c

    val my_type : bodypart_type = t

    method get_type = my_type

  end

(* a mob class *)
class base_creature (n : string) (c : container) =
  object (self)

    inherit creature
    inherit base_tangible

    val name = n
    val mutable parent = c
    val mutable body : bodypart list = []
    val mutable ctrl : controller = new dummy_controller

    method get_parent = parent

    method set_parent p = parent <- p

    method to_string = "base creature: " ^ name

    method view_contents looker pos =
      List.flatten (List.map (fun bp -> bp#view_contents looker pos) body)

    method get_contents pos =
      List.flatten (List.map (fun bp -> bp#get_contents pos) body)

    method matches_description sadjs sname =
      if sadjs <> [] then false
      else if starts_with name sname then true
      else false

    method short_description looker = MudString name

    method look_description looker =
      MudString name (* TODO: view contents here *)

    (* called to set some controller to be in control of this character *)
    method set_controller (c : controller) =
      ctrl <- c;
      ctrl#send_message (MudStringMeta (MetaInit, MudStringNone))

    (* called to pick up an object *)
    method take (thing : tangible) =
      dlog 4 "called take";
      if thing == (self : #creature :> tangible) then
        raise (Command_error "You can't pick up yourself.");
      if not (thing#can_be_gotten (self : #creature :> creature)) then
        raise (Command_error "You can't pick up that object.");
      let is_hand p =
        match p#get_type with
        | Hand _ -> true
        | _ -> false in
      let hands = List.filter is_hand body in
      dlog 0 "got hands";
      if List.exists (function h -> List.mem thing (h#get_contents (Some In)))
            hands
        then raise (Command_error "You are already holding that.");
      dlog 0 "checked hands";
      let rec take_one_handed = function
          h::hs -> (try thing#move (self : #creature :> creature) h In
            with Cannot_add _ -> take_one_handed hs)
        | [] -> raise (No_space_for thing) in
      dlog 4 ("creature is picking up " ^ (thing#to_string));
      take_one_handed (hands :> container list);
      dlog 4 "and got it.";
      (self#get_location)#relay_message (MudStringCondition
          ((self: #creature :> creature), MudStringList (SeparatorNone,
            [MudString "You pick up the "; MudStringName thing]),
          MudStringList (SeparatorNone,
            [MudStringName (self: #creature :> tangible);
            MudString " picked up the "; MudStringName thing])))

    method drop (thing : tangible) =
      (* FIXME: if two hands share a container to hold something,
           then this is won't work *)
      let get_holder bp =
        match bp#get_type with
        | Hand _ -> let contents = bp#view_contents (self :> creature) (Some In)
            in if List.mem thing contents then Some bp else None
	| _ -> None
      in
      let hand_containers = map_some get_holder body in
      if hand_containers = []
        then raise (Command_error "You aren't holding that.");
      thing#move (self :> creature) ((self#get_location) :> container) On;
      (self#get_location)#relay_message (MudStringCondition
          ((self: #creature :> creature), MudStringList (SeparatorNone,
            [MudString "You drop the "; MudStringName thing]), MudStringList
              (SeparatorNone, [MudStringName (self: #creature :> tangible);
              MudString " dropped the "; MudStringName thing])))

    method get_inventory (looker : creature) : inventory =
      let bp_inventory (bp : bodypart) =
        List.map (fun pos -> (bp#get_type, pos, bp#view_contents looker
              (Some pos)))
          [In; On]
      in List.flatten (List.map bp_inventory body)

    method is_shown looker = looker != (self : #creature :> creature)

    method is_visible looker = true

    method get_name = name

    method can_remove actor thing =
      List.exists (fun bp -> bp#can_remove actor thing) body

    method can_add actor thing where = false

    method can_be_gotten actor = false

    method add thing where = raise (Cannot_add thing)

    method remove thing =
      let rec remove_helper = function
        | bp::rest -> begin
              try bp#remove thing with
              | Cannot_remove _ -> remove_helper rest
            end
        | [] -> raise (Cannot_remove thing)
      in remove_helper body

    method private do_attack (target : tangible) (weapon : tangible)
        : mud_string =
      raise (Command_error "Command not implemented yet")

    method private do_wait (time : int) =
      let end_time = time + get_time () in
      dlog 3 ("someone is waiting until " ^ string_of_int end_time);
      dlog 3 ("it is now: " ^ string_of_int (get_time ()));
      let wait_thunk () =
        self#send_message (MudStringMeta (MetaWaitDone time, MudString
          ("After " ^ string_of_int time ^ " you grow tired of waiting."))) in
      event_list#register_event wait_thunk end_time;
      MudString "You begin waiting."

    method private do_move (p : portal) : mud_string =
      if p#can_pass (self :> tangible) then
        self#move (self :> creature) (p#dest :> container) On;
      MudStringList (SeparatorNone, [(MudString "You start walking...");
        (self#do_look (self#get_location :> mud_object))])

    method private do_look (target : mud_object) : mud_string =
      target#look_description (self : #creature :> creature)

    method private do_inventory () : mud_string =
      let inv_to_mud_string_list (stuff : tangible list) =
        MudStringList (SeparatorComma, List.map
            (function x -> MudStringName x) stuff) in
      let inv : inventory = self#get_inventory (self : #creature :> creature) in
      let wearing = List.flatten (List.map (fun (_, _, x) -> x) (List.filter
            (fun (_, x, _) -> x <> In) inv)) in
      let carrying = List.flatten (List.map (fun (_, _, x) -> x)
          (List.filter (function (Hand _, In, _) -> true | _ -> false) inv)) in
      let carrying_string = if carrying <> []
        then MudStringList (SeparatorNone, [MudString "You are carrying ";
           inv_to_mud_string_list carrying]) 
        else MudStringNone in
      let wearing_string = MudStringList (SeparatorNewline,
        [MudString "You are wearing:";
        if wearing <> [] then inv_to_mud_string_list wearing
          else MudString "nothing"]) in
      MudStringList (SeparatorNewline, [carrying_string; wearing_string])

    method private do_take (thing : tangible) : mud_string =
      try self#take thing; MudStringNone
      with
        | No_space_for _ -> MudString "Your hands are full."
        | Cannot_add _ -> MudString "You can't hold that."
        | Cannot_remove _ -> MudString "You cannot pick that up."

    method private do_drop (thing : tangible) : mud_string =
        self#drop thing; MudStringNone

    method private do_say (_, _, str) : mud_string =
      (self#get_location)#relay_message (MudStringCondition
        ((self: #creature :> creature),
          (MudString ("You say, \"" ^ str ^ "\"")),
          (MudStringList (SeparatorNone,
              [MudStringName (self: #tangible :> tangible);
              MudString (" says, \"" ^ str ^ "\"")]))));
      MudStringNone

    (* called by the controller to give the character commands *)
    method run_command (cmd : command) : unit =
      dlog 4 (name ^ " is running command " ^ string_of_cmd cmd);
      self#send_message (match cmd with
          Cmd_wait time -> self#do_wait time
        | Cmd_attack (x, y) -> self#do_attack x y
	| Cmd_move x    -> self#do_move x
	| Cmd_look x    -> self#do_look x
	| Cmd_take x	-> self#do_take x
        | Cmd_drop x    -> self#do_drop x
        | Cmd_inventory -> self#do_inventory ()
        | Cmd_say x     -> self#do_say x)

    (* method called by the world and other objects when some stimulus
       effects the character *)
    method send_message (msg: mud_string) : unit = ctrl#send_message msg

    method add_bodypart (bp : bodypart) : unit =
      body <- bp::body

  end

class hand (s : side) (c : container) =
  object (self)
    inherit bodypart (Hand s) c

    val mutable contents_in : tangible option = None
    val mutable contents_on : tangible option = None

    method to_string =
      "hand container for: " ^ (parent#to_string)

    method can_add (actor : creature) (thing : tangible) (where : position)
        : bool =
      match where with
      | In -> Option.is_none contents_in
      | On -> Option.is_none contents_on
      | _ -> false

    method add (thing : tangible) (where : position) : unit =
      match where with
      | In -> if Option.is_some contents_in then raise (Cannot_add thing)
          else contents_in <- Some thing
      | On -> if Option.is_some contents_on then raise (Cannot_add thing)
          else contents_on <- Some thing
      | _ -> raise (Cannot_add thing)

    method can_remove (actor : creature) (thing : tangible) : bool =
      contents_in = Some thing || contents_on = Some thing

    method remove (thing : tangible) : unit =
      if contents_in = Some thing then contents_in <- None
      else if contents_on = Some thing then contents_on <- None
      else raise (Cannot_remove thing)

    method get_contents (where : position option) : tangible list =
      match where with
      | Some In -> option_to_list contents_in
      | Some On -> option_to_list contents_on
      | Some _ -> []
      | None -> (option_to_list contents_in)@(option_to_list contents_on)

    method view_contents (looker : creature) (where : position option)
        : tangible list =
      List.filter (fun t -> t#is_visible looker) (self#get_contents where)

  end
