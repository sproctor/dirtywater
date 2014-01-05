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

(* main : This is the main game file. Running _ starts the game running by
   starting the server on a separate thread to accept connections and then
   starting the game scheduler to which is the main event loop of the game.
   This file is also responcible for command line argument, command line
   usage messages, versioning information, and help messages. *)

(* global status of whether the game is up or not *)
let game_running = ref true

(* start up the main server in another thread and start the scheduler *)
let _ =
  Sys.catch_break true;
  let port = ref 4000 in
  let sp = [("--debug", Arg.Int Debug.set_debug,
             "Set the verbosity of the log messages, higher more verbose");
            ("--log", Arg.String Debug.set_log_file,
             "File name to write log messages to. defaults to standard out")] in
  let set_port n =
    try
      port := (int_of_string n)
    with Failure _ -> (Arg.usage sp "Bad port number"; exit 1)
  in
  Arg.parse sp set_port "mud [options] port";
  Debug.dlog 2 ("verbosity set to " ^ string_of_int !Debug.debug_level);
  let sock = Server.start_server !port in
  begin try
    while true do
      let delay = Scheduler.iterate_scheduler () in
      Server.iterate_server sock delay
    done
  with
    Sys.Break ->
      Debug.dlog 0 "Got break"
    | x ->
      Debug.dlog 0 (Printexc.to_string x)
  end;
  Server.disconnect sock;
  Debug.dlog 0 "Stopping mud server"
