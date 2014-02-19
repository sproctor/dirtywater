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
   events.ml: this file contains the event class. an event is a thunk that
   is scheduled to execute at a certain time
*)

open Time
open Base

class timed_event_list =
  object (self)
    val mutable events_list = []
    method register_event (func : unit -> unit) (time : int) : unit =
      dlog 4 "register_event: <-- in here";
      let rec add_in_time el =
        match el with
            (f, t)::rest -> dlog 4 ("time is " ^ string_of_int (get_time ())
                            ^ ", t is " ^ string_of_int t);
                            if time > t then (f, t)::(add_in_time rest)
                            else (func, time)::el
          | []           -> [(func, time)]
      in events_list <- add_in_time events_list
    method run_events () : unit =
      match events_list with
          (f, t)::rest ->
            if t <= get_time () then (events_list <- rest;
                                      f (); dlog 3 "ran an event";
                                      self#run_events ())
        | [] -> ()
  end

let event_list = new timed_event_list
