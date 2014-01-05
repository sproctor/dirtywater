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
open Helpers
open Character
open Debug
open State
open Base

(* a class to maintain the connection and give events to the scheduler *)
class player (ch : iCharacter) (co : iConnection) =
  object (self)
    (* must register the controller with the scheduler in base class *)
    inherit iPlayer
    inherit iController
    val controllee = ch
    val mutable cmd_queue = []
    val conn = co
    val mutable prompting = false
    (* send a message to the user's connection. returns immediatly *)
    (* called by scheduler *)
    method send_messages (msgs : message list) : unit = 
      let send_message (msg_data, msg_text) =
        dlog 4 "sent a message";
        if (String.length msg_text) > 0 then begin
          if prompting then (conn#output "\r\n"; prompting <- false);
          conn#output (msg_text ^ "\r\n");
        end;
        match msg_data with
            Msg_prompt -> conn#output ("> "); prompting <- true
          | Msg_init   -> cmd_queue <- cmd_queue@[Player_look None]
          | _          -> () in
      dlog 5 "start player->send message";
      List.iter send_message msgs;
      send_message (Msg_prompt, "");
      dlog 5 "end player->send message";
    method private translate_command (pcmd : player_command) : command =
      match pcmd with
          Player_attack (ptarget, pweapon) ->
	    raise (Bad_command "Not yet implemented")
	| Player_wait (Some x) ->
            Cmd_wait x
	| Player_wait None ->
            Cmd_wait 10
	| Player_quit ->
           raise Quit
	| Player_move ed ->
	    let room = controllee#get_location in
            let (exit_obj, error) = match ed with
                ExitDescDir dir -> (ExitDir dir, Direction_not_valid dir)
              | ExitDescObj desc ->
                let obj = controllee#look_for desc in
                (ExitObj obj, Object_not_exit obj) in
            let port = room#get_exit exit_obj in
	    Cmd_move (match port with Some y -> y
	      | None -> raise error)
        | Player_inventory ->
            Cmd_inventory
	| Player_look None ->
	    Cmd_look (LocationObject controllee#get_location)
	| Player_look Some (None, desc) ->
            Cmd_look (TangibleObject (controllee#look_for desc))
        | Player_look Some (Some prep, desc) ->
            Cmd_look (PrepositionObject (prep, TangibleObject
                  (controllee#look_for desc)))
        | Player_take desc -> 
            Cmd_take (controllee#look_for desc)
        | Player_drop desc ->
            Cmd_drop (controllee#look_for desc)
    (* return the next command in the form (option cmd). if there is no command
       return (none) *)
    method run_command () : unit =
      match cmd_queue with
          pcmd::rest ->
            cmd_queue <- rest;
	    begin try
	      let cmd = self#translate_command pcmd in
	      controllee#run_command cmd
	      with
	        | Object_not_found (desc, _) -> self#send_messages [(Msg_nop,
		    "I couldn't find the " ^ object_desc_to_string desc ^ ".")]
                | Object_not_exit o -> self#send_messages [(Msg_nop,
                    "You cannot go into the " ^ o#get_name ^ ".")]
                | Direction_not_valid dir -> self#send_messages [(Msg_nop,
		    "You can't go " ^ List.assoc dir direction_list ^ " here.")]
                | Bad_command str -> self#send_messages [(Msg_nop,
                    "Bad command: " ^ str)]
                | Command_error str -> self#send_messages [(Msg_nop, str)]
		| Quit -> self#logout ()
		| x -> self#send_messages [(Msg_nop, "Unhandled exception: "
                      ^ Printexc.to_string x)]
	    end
        | [] -> ()
    method private logout () =
      controllee#set_controller (new Ai.simple_ai controllee);
      dlog 4 "logging out";
      current_players#remove_player (self : #iPlayer :> iPlayer);
      conn#output "\r\nFarewell.\r\n";
      conn#close ();
    method enqueue_command (str : string) : unit =
      try
        let cmd = Lexer.parse_command str in
        cmd_queue <- cmd_queue@[cmd]
      with
          Bad_command str -> conn#output (str ^ "\r\n")
        | _ -> ()
    initializer
      controllee#set_controller (self : #iPlayer :> iController);
      current_players#add_player (self : #iPlayer :> iPlayer)
  end
