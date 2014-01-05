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
   main : This is the main game file. _ contains the main game loop.
   the game loop iterates the scheduler and processes one line of input from
   each player.
   This file is also responsible for command line arguments, command line
   usage messages, versioning information, and help messages.
*)

open Debug
open Scheduler
open Server

let _ =
  Sys.catch_break true;
  let port = ref 4000 in
  let sp = [("--debug", Arg.Int set_debug,
             "Set the verbosity of the log messages, higher more verbose");
            ("--log", Arg.String set_log_file,
             "File name to write log messages to. defaults to standard out")] in
  let set_port n =
    try
      port := (int_of_string n)
    with Failure _ -> (Arg.usage sp "Bad port number"; exit 1)
  in
  Arg.parse sp set_port "mud [options] port";
  dlog 2 ("verbosity set to " ^ string_of_int !debug_level);
  Load.load_tangibles ();
  Load.load_locations ();
  let sock = Server.start_server !port in
  begin try
    while true do
      let delay = iterate_scheduler () in
      iterate_server sock delay
    done
  with
    Sys.Break ->
      dlog 0 "Got break"
    | x ->
      dlog 0 (Printexc.to_string x)
  end;
  disconnect sock;
  dlog 0 "Stopping mud server"
