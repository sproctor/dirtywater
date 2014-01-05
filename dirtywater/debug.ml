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

 debug.ml: logging/debugging helper funtions go here
 *)

let debug_level = ref 0

(* ... set the debug/verbosity level *)
let set_debug (n : int) : unit =
  debug_level := n

let log_channel = ref stdout

(* ... set the log file *)
let set_log_file (str : string) : unit =
  log_channel := open_out str;
  print_string ("writing to " ^ str); print_newline ()

(* log the string if if the verbosity is at least d_level *)
let dlog (d_level : int) (str : string) =
  if d_level <= !debug_level then
    begin
      output_string !log_channel (str ^ "\n");
      flush !log_channel
    end
