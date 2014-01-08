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
    | Cmd_say _ -> "say"

(* basic character class *)
class character (i : int) (n : string) (p : string) (b : bodypart) =
  object (self)

    inherit iCharacter
    inherit creature i n b

    val mutable password = p

    method private do_attack (target : iTangible) (weapon : iTangible)
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

    method private do_move (p : iPortal) : mud_string =
      if p#can_pass (self : #iCharacter :> iTangible) then
        self#move_to (p#dest :> iContainer);
      MudStringList (SeparatorNone, [(MudString "You start walking...");
        (self#do_look (LocationObject self#get_location))])

    method private do_look (target : object_type) : mud_string =
      match target with
        | TangibleObject x -> x#get_long_desc (self : #iCharacter :> iCreature)
	| LocationObject x -> x#get_description
            (self : #iCharacter :> iCreature)
        | _ -> raise (Command_error "This type of look isn't implemented.")

    method private do_inventory () : mud_string =
      let i : inventory = self#get_inventory (self : #iCreature :> iCreature) in
      let inv = List.flatten (List.map (function (_, _, x) -> x) (List.filter
            (function (Hand _, _, _) -> false | (_, x, _) -> x <> In) i)) in
      let carrying = List.flatten (List.map (function (_, _, x) -> x)
          (List.filter (function (Hand _, In, _) -> true | _ -> false) i)) in
      let carrying_string = if carrying <> []
        then MudStringList (SeparatorNone, [MudString "You are carrying ";
            MudStringList (SeparatorComma, List.map
            (function x -> MudStringName x) carrying)])
        else MudStringNone in
      let inv_string = MudStringList (SeparatorNewline,
        [MudString "You are wearing:";
        if inv <> [] then MudStringList (SeparatorComma,
          List.map (function x -> MudStringName x) inv)
          else MudString "nothing"]) in
      MudStringList (SeparatorNewline, [carrying_string; inv_string])

    method private do_take (thing : iTangible) : mud_string =
      try self#take thing; MudStringNone
      with
        | No_space_for _ -> MudString "Your hands are full."
        | Cannot_add _ -> MudString "You can't hold that."
        | Cannot_remove _ -> MudString "You cannot pick that up."

    method private do_drop (thing : iTangible) : mud_string =
        self#drop thing; MudStringNone

    method private do_say (_, _, str) : mud_string =
      (self#get_location)#relay_message (MudStringCondition
        ((self: #iCharacter :> iCreature),
          (MudString ("You say, \"" ^ str ^ "\"")),
          (MudStringList (SeparatorNone,
              [MudStringName (self: #iCharacter :> iTangible);
              MudString (" says, \"" ^ str ^ "\"")]))));
      MudStringNone

    (* called by the controller to give the character commands *)
    method run_command (cmd : command) : unit =
      dlog 4 (name ^ " is running command " ^ cmd_to_string cmd);
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

    (* called by the login to check if the given password is right *)
    method match_password (guess : string) : bool = (guess = password)

    method set_password p =
      password <- p

    initializer
      active_characters#add name (self : #iCharacter :> iCharacter)
  end
