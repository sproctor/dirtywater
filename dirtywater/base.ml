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

(* base object that the rest of the objects in the mud should be derived from *)
class virtual mud_object =
  object
    inherit iMud_object
    method as_creature = None
  end

let noun_desc_to_string ((ord, adjs, noun) : noun_desc) =
  (string_of_int (get_opt_default ord 1)) ^ " "
    ^ String.concat " " (adjs@[noun])

let preposition_to_string prep =
  match prep with
      Prep_under -> "under"
    | Prep_on    -> "on"
    | Prep_in    -> "in"
    | Prep_from  -> "from"
    | Prep_of    -> "of"
    | Prep_any   -> "anywhere"

let preposition_to_containment_option (prep : preposition)
    : (containment option)=
  match prep with
      Prep_on -> Some On
    | Prep_in -> Some In
    | _       -> None

let rec object_desc_to_string desc =
  match desc with
      ObjectDesc (next, prep, noun) -> object_desc_to_string next ^ " "
          ^ preposition_to_string prep ^ " " ^ noun_desc_to_string noun
    | ObjectDescRelative (next, prep) -> object_desc_to_string next ^ " "
          ^ preposition_to_string prep
    | ObjectDescBase noun -> noun_desc_to_string noun

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
  else if part = "leg" then Leg (side_of_string (get_opt s))
  else if part = "arm" then Arm (side_of_string (get_opt s))
  else if part = "hand" then Hand (side_of_string (get_opt s))
  else if part = "foot" then Foot (side_of_string (get_opt s))
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
