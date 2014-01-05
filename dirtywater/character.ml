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

(* character.ml : This file contains the base class for character from which
   all other character (players and nonplayers alike) are dervived. The
   character base class provides the interaction with the controller and
   implementations for the difference actions that can be called on a
   character. subclass will inherit from character and change values of private
   skill fields to make races diverse *)

open Types
open Debug
open Helpers
open Base
open State
open Time
open Creature

let cmd_to_string cmd =
  match cmd with
      Cmd_wait x -> "wait " ^ string_of_int x
    | Cmd_attack _ -> "attack"
    | Cmd_move _ -> "move"
    | Cmd_look _ -> "look"
    | Cmd_take _ -> "take"
    | Cmd_drop _ -> "drop"
    | Cmd_inventory -> "inventory"

(* basic character class *)
class character (n : string) (p : string) (b : iBodypart) =
  object (self)
    inherit iCharacter
    inherit creature [] n b
    val mutable password = p
    method private do_attack (target : iTangible) (weapon : iTangible) =
      ctrl#send_messages [(Msg_nop, "Attacking isn't implemented yet.")]
    method private do_wait (time : int) : unit =
      let end_time = time + get_time () in
      dlog 3 ("someone is waiting until " ^ string_of_int end_time);
      dlog 3 ("it is now: " ^ string_of_int (get_time ()));
      let wait_thunk () =
        self#send_messages [((Msg_wait_end time), "After "
	  ^ string_of_int time ^ " you grow tired of waiting.")]
      in event_list#register_event wait_thunk end_time;
      ctrl#send_messages [((Msg_wait_start time), "You begin waiting.")]
    method private do_move (p : iPortal) : unit =
      if p#can_pass (self : #iCharacter :> iTangible) then
        self#move_to [((p#dest :> iContainer), Anywhere)];
      ctrl#send_messages [(Msg_nop, "You start walking...")];
      self#do_look (LocationObject self#get_location)
    method private do_look (target : object_type) =
      match target with
        | TangibleObject x -> ctrl#send_messages [(Msg_nop, x#get_long_desc
              (self : #iCharacter :> iCreature))]
	| LocationObject x -> ctrl#send_messages [(Msg_nop, x#get_description
	    (self : #iCharacter :> iCreature))]
        | _ -> raise (Command_error "This type of look isn't implemented.")
    method private do_inventory () =
      let i : inventory = self#get_inventory in
      let inv = List.flatten (List.map (function (_, _, x) -> x) (List.filter
            (function (Hand _, _, _) -> false | (_, x, _) -> x <> In) i)) in
      let strings = List.map (function obj -> obj#get_short_desc
        (self : #iCharacter :> iCreature)) inv in
      let carrying = List.flatten (List.map (function (_, _, x) -> x)
          (List.filter (function (Hand _, In, _) -> true | _ -> false) i)) in
      let carry_strings = List.map (function obj -> obj#get_short_desc
        (self : #iCharacter :> iCreature)) carrying in
      ctrl#send_messages ((if carry_strings <> [] then [(Msg_nop,
            "You are carrying " ^ (add_commas carry_strings))] else [])
        @[(Msg_nop, "You are wearing:");
          (Msg_nop, if strings <> [] then (add_commas strings) else "nothing")])
    method private do_take (thing : iTangible) =
      try
        self#take thing;
        ctrl#send_messages [(Msg_nop, "You picked up the "
              ^ thing#get_short_desc (self : #iCharacter :> iCreature))]
      with
        | No_space_for _ ->
            ctrl#send_messages [(Msg_nop, "Your hands are full.")]
        | Cannot_add _ -> ctrl#send_messages [(Msg_nop,
              "You can't hold that.")]
        | Cannot_remove _ -> ctrl#send_messages [(Msg_nop,
              "You cannot pick that up.")]
    method private do_drop (thing : iTangible) =
        self#drop thing;
        ctrl#send_messages [(Msg_nop, "You dropped the "
            ^ thing#get_short_desc (self : #iCharacter :> iCreature))]
    (* called by the controller to give the character commands *)
    method run_command (cmd : command) : unit =
      dlog 4 (self#get_name ^ " is running command " ^ cmd_to_string cmd);
      match cmd with
          Cmd_wait time -> self#do_wait time
        | Cmd_attack (x, y) -> self#do_attack x y
	| Cmd_move x    -> self#do_move x
	| Cmd_look x    -> self#do_look x
	| Cmd_take x	-> self#do_take x
        | Cmd_drop x    -> self#do_drop x
        | Cmd_inventory -> self#do_inventory ()
    (* method called by the world and other objects when some stimulus
       effects the character *)
    method send_messages (msgs : message list) : unit = ctrl#send_messages msgs
    (* called by the login to check if the given password is right *)
    method match_password (guess : string) : bool = ((=) guess password)
    method can_add prep thing = true
    method add prep thing = ()
    initializer
      active_characters#add_character (self : #iCharacter :> iCharacter)
  end

let make_character name password start =
  let head = new bodypart Head Head [Torso] in
  let ch = new character name password head in
  head#move_to [((ch :> iContainer), Anywhere)];
  let connect_to this that =
    this#move_to [((ch :> iContainer), Anywhere)];
    this#attach_to that;
    that#receive this in
  let torso = new bodypart Torso Head
    [(Arm Left);(Arm Right);(Leg Left);(Leg Right)] in
  connect_to torso head;
  let left_arm = new bodypart (Arm Left) Torso [(Hand Left)] in
  connect_to left_arm torso;
  let left_hand = new hand Left in
  connect_to left_hand left_arm;
  let right_arm = new bodypart (Arm Right) Torso [(Hand Right)] in
  connect_to right_arm torso;
  let right_hand = new hand Right in
  connect_to right_hand right_arm;
  let left_leg = new bodypart (Leg Left) Torso [(Foot Left)] in
  connect_to left_leg torso;
  let left_foot = new bodypart (Foot Left) (Leg Left) [] in
  connect_to left_foot left_leg;
  let right_leg = new bodypart (Leg Right) Torso [(Foot Right)] in
  connect_to right_leg torso;
  let right_foot = new bodypart (Foot Right) (Leg Right) [] in
  connect_to right_foot right_leg;
  ch#move_to [((start :> iContainer), Anywhere)];
  ch
