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
open Debug

class character_collection =
  object
    inherit iCharacter_collection
    val mutable character_list = ([] : iCharacter list)
    (* adds a character to the list *)
    method add_character (ch : iCharacter) : unit =
      dlog 2 "adding character to list";
      character_list <- ch::character_list
    (* finds a character in the list of characters. returns Some character if
       it did or None if it didn't *)
    method find_character_by_name (name : string) : iCharacter option =
      let rec find_character_in_list characters =
        match characters with
            [] -> None
          | ch::chs -> if(name = ch#get_name)
                       then Some ch
                       else find_character_in_list chs
      in find_character_in_list character_list
  end
