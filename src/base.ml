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
 base.ml : this file contains the base class for the mud objects and
   very generic functions on data types.
 *)

open Types
open Helpers

let direction_list = [
    (Down, "down");
    (East, "east");
    (North, "north");
    (NorthEast, "northeast");
    (NorthWest, "northwest");
    (South, "south");
    (SouthEast, "southeast");
    (SouthWest, "southwest");
    (Up, "up");
    (West, "west");
  ]

let direction_lookup_list = [
    ("down", Down);
    ("east", East);
    ("north", North);
    ("northeast", NorthEast);
    ("ne", NorthEast);
    ("northwest", NorthWest);
    ("nw", NorthWest);
    ("south", South);
    ("southeast", SouthEast);
    ("se", SouthEast);
    ("southwest", SouthWest);
    ("sw", SouthWest);
    ("up", Up);
    ("west", West);
  ]

let string_of_ordinal (ord : int) : string =
  let mod100 = ord mod 100 in
  let mod10 = ord mod 10 in
  let suffix =
    if ord < 1 then raise (Failure "ordinal less than 1")
    else if mod100 = 11 then "th"
    else if mod100 = 12 then "th"
    else if mod100 = 13 then "th"
    else if mod10 = 1 then "st"
    else if mod10 = 2 then "nd"
    else if mod10 = 3 then "rd"
    else "th"
  in
  (string_of_int ord) ^ suffix

let noun_desc_to_string ((ord, adjs, noun) : noun_desc) =
  let ord_string =
    match ord with
    | Some o -> (string_of_ordinal o) ^ " "
    | None -> ""
  in
  ord_string ^ (String.concat " " (adjs@[noun]))

let preposition_to_string prep =
  match prep with
    | Prep_on     -> "on"
    | Prep_in     -> "in"
    | Prep_any    -> "any"
    | Prep_behind -> "behind"
    | Prep_under  -> "under"

let preposition_to_position ((prep : preposition), (thing : tangible))
    : (position option * container) =
  match prep with
    | Prep_on -> (Some On, (thing :> container))
    | Prep_in -> (Some In, (thing :> container))
    | Prep_under -> (Some (Under thing), thing#get_parent)
    | Prep_behind -> (Some (Behind thing), thing#get_parent)
    | Prep_any -> (None, (thing :> container))

let rec object_desc_to_string desc =
  match desc with
      ObjectDesc (next, prep, noun) -> object_desc_to_string next ^ " "
          ^ preposition_to_string prep ^ " " ^ noun_desc_to_string noun
    | ObjectDescBase noun -> noun_desc_to_string noun

let string_of_position (pos : position) =
  match pos with
  | In -> "in"
  | On -> "on"
  | Under _ -> "under"
  | Behind _ -> "behind"

let side_to_string (s : side) =
  match s with
      Left -> "left"
    | Right -> "right"

let bodypart_to_desc (bp : bodypart_type) =
  match bp with
      Head -> ([], "head")
    | Leg s -> ([side_to_string s], "leg")
    | Arm s -> ([side_to_string s], "arm")
    | Hand s -> ([side_to_string s], "hand")
    | Foot s -> ([side_to_string s], "foot")
    | Torso -> ([], "torso")

let side_of_string (str : string) : side =
  if str = "left" then Left
  else if str = "right" then Right
  else assert false

let bodypart_type_of_string (part : string) (s : string option)
    : bodypart_type =
  if part = "head" then (assert (s = None); Head)
  else if part = "leg" then Leg (side_of_string (Option.get s))
  else if part = "arm" then Arm (side_of_string (Option.get s))
  else if part = "hand" then Hand (side_of_string (Option.get s))
  else if part = "foot" then Foot (side_of_string (Option.get s))
  else if part = "torso" then (assert (s = None); Torso)
  else assert false

let emote_lookup = [
    ("quietly", EmoteQuietly);
    ("loudly", EmoteLoudly);
  ]

let string_to_emote str =
  assoc_fun (start_of str) emote_lookup

let direction_of_string str =
  assoc_fun (start_of str) direction_lookup_list

let string_of_direction dir =
  List.assoc dir direction_list

let string_of_cmd cmd =
  match cmd with
  | Cmd_wait x -> "wait " ^ string_of_int x
  | Cmd_attack _ -> "attack"
  | Cmd_move _ -> "move"
  | Cmd_look _ -> "look"
  | Cmd_look_position _ -> "look"
  | Cmd_take _ -> "take"
  | Cmd_drop _ -> "drop"
  | Cmd_inventory -> "inventory"
  | Cmd_say _ -> "say"

let ignore_tangible (_ : tangible) = ()

let debug_level = ref 0

(* ... set the debug/verbosity level *)
let set_debug (n : int) : unit =
  debug_level := n

let log_channel = ref stdout

(* ... set the log file *)
let set_log_file (str : string) : unit =
  log_channel := open_out str;
  print_endline ("writing to " ^ str)

(* log the string if if the verbosity is at least d_level *)
let dlog (d_level : int) (str : string) =
  if d_level <= !debug_level then
    begin
      output_string !log_channel (str ^ "\n");
      flush !log_channel
    end

