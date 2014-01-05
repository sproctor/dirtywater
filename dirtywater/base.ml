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

 mud_object.ml : this file contains the base class for the mud objects
 *)

open Types
open Helpers

(* base object that the rest of the objects in the mud should be derived from *)
class virtual mud_object =
  object
    inherit iMud_object
  end

let noun_desc_to_string ((ord, adjs, noun) : noun_desc) =
  (string_of_int (get_opt_default ord 1)) ^ " "
    ^ String.concat " " (adjs@[noun])

let preposition_to_string prep =
  match prep with
      Under -> "under"
    | On    -> "on"
    | In    -> "in"
    | From  -> "from"
    | Of    -> "of"
    | Between (n1, n2) -> "between " ^ noun_desc_to_string n1 ^ " and "
        ^ noun_desc_to_string n2
    | Near noun -> "near " ^ noun_desc_to_string noun
    | Anywhere -> "anywhere"

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
