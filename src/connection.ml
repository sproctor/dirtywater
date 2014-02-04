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
   connection.ml: this file contains the data structures for the connection
   for each player to the server. connections have a state. whether someone
   is logging in, logged in, etc is stored in the connection.
*)

open Helpers
open Debug
open Types
open Character
open Player
open Connection_collection
open Character_collection
open Location_collection

type login_state = {
  mutable name: string option;
  mutable confirm_name: bool;
  mutable password: string option;
}

type connection_state =
  | Login of login_state
  | Playing of player

class telnet_connection s =
  object (self)
    inherit connection
    val sock = s
    val tmp_buffer = String.create 4096
    val in_buffer = Buffer.create 32
    val out_buffer = Buffer.create 32
    val mutable cmd_strings = []
    val mutable state = Login {name = None; confirm_name = false;
        password = None;}

    method get_descriptor = sock

    (* closes the connection *)
    method close () : unit =
      dlog 4 "closing connection...";
      Unix.close sock;
      connections#remove (self : #connection :> connection);
      dlog 4 "closed down connection";

    method private step_login cmd lstate =
      if lstate.name = None then (
        lstate.name <- Some cmd;
        lstate.confirm_name <- false;
        self#output (cmd ^
            " doesn't exist. Are you sure you want to take this name? [y/n] ")
      ) else if lstate.confirm_name = false then (
        if start_of (String.lowercase cmd) "yes" then (
          lstate.confirm_name <- true;
          lstate.password <- None;
          self#output "What is your password? "
        ) else (
          lstate.name <- None;
          self#output "What name would you go by then? "
        )
      ) else (
        lstate.password <- Some cmd;
        let ch = try
          let c = active_characters#find_character_by_name
            (Option.get lstate.name) in
          if not (c#match_password (Option.get lstate.password)) then
              (lstate.name <- None; self#output "Invalid password\r\n");
          c
        with _ -> (
            dlog 4 ("Creating character: " ^ Option.get lstate.name);
            make_character (Option.get lstate.name) (Option.get lstate.password)
            ) in
        dlog 4 "creating creature";
        let cr = ch#create_creature ((locations#get 1001) :> container) in
        let p = new normal_player cr (self : #connection :> connection) in
        state <- Playing p
      )
    method input () : unit =
      let process_input () =
        let num_read = Unix.read sock tmp_buffer 0 4096 in
        if num_read = 0 then self#close ();
        Buffer.add_substring in_buffer tmp_buffer 0 num_read;
        if Buffer.length in_buffer > 4096 then self#close ();
        let str = Buffer.contents in_buffer in
        dlog 4 ("buffer is: " ^ str);
        match Str.split_delim (Str.regexp "\r\n") str with
            [] -> Buffer.reset in_buffer
          | strs ->
            let rec add_strings = function
                []    -> Buffer.reset in_buffer
              | x::[] -> Buffer.reset in_buffer;
                  Buffer.add_string in_buffer x
              | x::xs -> cmd_strings <- cmd_strings@[x];
                  add_strings xs in
            add_strings strs in
      if cmd_strings = [] then process_input ();
      match cmd_strings with
        | [] -> ()
        | cmd::remaining_cmds -> begin
            cmd_strings <- remaining_cmds;
            match state with
              | Login lstate ->
                  self#step_login cmd lstate
              | Playing player ->
                  player#enqueue_command (telnet_to_ascii cmd)
          end
    (* writes the string to the connection, escaped any telnet data stuff *)
    method output (str : string) : unit =
      (*out_buffer <- out_buffer ^ str;*)
      let len = String.length str in
      if len > 10000 then self#close ()
      else ignore (Unix.write sock (ascii_to_telnet str) 0 len)
    initializer
      self#output "Welcome. What name do you go by? "
  end

