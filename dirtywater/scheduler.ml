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

(* scheduler.ml : This file contains the scheduler function, which is the
   main event loop for the game. The whole game takes place inside the
   schduler which is started by start_schduler. The schduler is an infinite
   loop which prompts each player in the game to execute one of his or her
   commands if one is pending. The scheduler is also incharge of invoking
   delayed effects which are to be executed some time in the future. *)

open Debug
open Time

(* sorted list of event to take place in the future *)
(* let pending_events = ref []*)

(* global state whether the game is running or not *)
let game_running = ref true

(* game time of when the scheduler was last run *)
let t = ref (get_time_float ())

(* iterate the main game loop that controlls game time and events *)
let iterate_scheduler () : float =
  let time_delta = get_time_float () -. !t in
  dlog 6 ((string_of_float time_delta) ^ " seconds have passed");
  let time_until_iteration = 0.25 -. time_delta in
  if time_until_iteration <= 0.0 then (
    t := get_time_float ();
    State.event_list#run_events ();
    State.current_players#run_commands ();
    0.0
  ) else time_until_iteration
