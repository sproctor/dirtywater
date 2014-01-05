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
   character_collection.ml: this file contains a class for collecting characters
   any actions that affect all characters should be implemented here. any
   character created should be added to this list
*)

open Types
open Debug

class character_collection =
  object
    val mutable character_list: (string * iCharacter) list = []
    (* adds a character to the list *)
    method add (name: string) (ch: iCharacter) : unit =
      dlog 2 "adding character to list";
      if List.exists (function (n, _) -> n = name) character_list then
        raise (Failure "add_character");
      character_list <- (name, ch)::character_list
    (* finds a character in the list of characters. returns Some character if
       it did or None if it didn't *)
    method find_character_by_name (name : string) : iCharacter =
      List.assoc name character_list
  end
