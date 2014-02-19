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

(* player_collection.ml: this file contains the list of players. Any methods
   that affect all players should be defined here. run_commands runs one
   command for each player
*)

open Types
open Base

class player_collection =
  object
    val mutable player_list = ([] : player list)

    (* registers a player in the player list *)
    method add (p : player) : unit =
      dlog 3 "registering player";
      player_list <- p::player_list

    (* unregister a player from the player list,
       that player's controller must be defunct, or we will block *)
    method remove (p : player) : unit =
      dlog 3 ("before removal: " ^ string_of_int (List.length player_list));
      let old_list = player_list in
      player_list <- List.filter ((!=) p) player_list;
      dlog 3 ("after removal: " ^ string_of_int (List.length player_list));
      if List.length player_list <> List.length old_list - 1 then
        raise (Failure "Problem removing player")

    (* runs one command from each player in the list *)
    method run_commands () : unit =
      let rec run_player_commands pl =
        match pl with
            p::ps -> p#run_command (); run_player_commands ps
          | []    -> ()
      in run_player_commands player_list;
  end

let current_players = new player_collection
