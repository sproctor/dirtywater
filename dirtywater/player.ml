(*
 Copyright 2014, 2003 Sean Proctor, Mike MacHenry

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

(* player.ml: this file contains the player class. a player is kind of
   an intermediate class beteen a connection and a character.
   A player is the controller for a human character.
*)
open Types
open Helpers
open Character
open Debug
open Base
open Lexer
open Player_collection
open Race_collection
open Container

(* TODO: move this function and the find functions into their own file *)
let look_for (looker : creature) (desc : object_desc) : tangible =
  try find looker (looker :> container) None desc with
  | Object_not_found (_, num) -> find looker
        (looker#get_location :> container) None desc

(* a class to maintain the connection and give events to the scheduler *)
(* normal player as opposed to admin *)
class normal_player (cr : creature) (co : connection) =
  object (self)
    (* must register the controller with the scheduler in base class *)
    inherit controller

    val controllee = cr
    val mutable cmd_queue = []
    val conn = co
    val mutable prompting = false

    (* send a message to the user's connection. returns immediatly *)
    (* called by scheduler *)
    method send_message (msg: mud_string) : unit = 
      dlog 5 "start player->send message";
      conn#output (Mud_string.to_string (controllee :> creature)
          (MudStringList (SeparatorNewline,
            [msg; MudStringMeta (MetaPrompt, MudStringNone)])));
      dlog 5 "end player->send message";

    method private translate_command (pcmd : player_command) : command =
      match pcmd with
      | Player_attack (ptarget, pweapon) ->
          raise (Bad_command "Not yet implemented")
      | Player_wait (Some x) ->
          Cmd_wait x
      | Player_wait None ->
          Cmd_wait 10
      | Player_quit ->
          raise Quit
      | Player_move ed ->
          let room = controllee#get_location in
          let (exit_obj, error) =
            match ed with
            | ExitDescDir dir -> (ExitDir dir, Direction_not_valid dir)
            | ExitDescObj desc ->
                let obj = Container.find (controllee :> creature)
                  (room :> container) None desc in
                (ExitObj obj, Object_not_exit obj) in
                let portal_opt = room#get_exit exit_obj in
                let portal = (match portal_opt with Some y -> y
                    | None -> raise error) in
	        Cmd_move portal
        | Player_inventory ->
            Cmd_inventory
	| Player_look None ->
	    Cmd_look (controllee#get_location :> mud_object)
        | Player_look Some (prep, desc) ->
            let desc_thing = look_for controllee desc in
            let (pos_opt, thing) = preposition_to_position (prep, desc_thing) in
            begin
              match pos_opt with
              | Some pos ->
                  Cmd_look_position (look_for controllee desc, pos)
              | None -> Cmd_look ((look_for controllee desc) :> mud_object)
            end
        | Player_take desc -> 
            (dlog 4 ("taking " ^ (object_desc_to_string desc));
            Cmd_take (look_for controllee desc))
        | Player_drop desc ->
            Cmd_drop (look_for controllee desc)
        | Player_say (es, _, str) ->
            Cmd_say (es, [], str)
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
	        | Object_not_found (desc, _) -> self#send_message (MudString
		    ("I couldn't find the " ^ object_desc_to_string desc ^ "."))
                | Object_not_exit o -> self#send_message (MudString
                    ("You cannot go into the " ^ o#get_name ^ "."))
                | Direction_not_valid dir -> self#send_message (MudString
		    ("You can't go " ^ string_of_direction dir ^ " here."))
                | Bad_command str -> self#send_message (MudString
                    ("Bad command: " ^ str))
                | Command_error str -> self#send_message (MudString str)
		| Quit -> self#logout ()
		| x -> self#send_message (MudString ("Unhandled exception: "
                      ^ Printexc.to_string x))
	    end
        | [] -> ()

    method private logout () =
      controllee#set_controller (new Ai.simple_ai controllee);
      dlog 4 "logging out";
      current_players#remove (self : #player :> player);
      conn#output "\r\nFarewell.\r\n";
      conn#close ();

    method enqueue_command (str : string) : unit =
      try
        let cmd = parse_command str in
        cmd_queue <- cmd_queue@[cmd]
      with
        | Bad_command str -> conn#output (str ^ "\r\n")
        | _ -> ()

    initializer
      controllee#set_controller (self : #player :> controller);
      current_players#add (self : #player :> player)
  end

let make_character name password =
  let r = races#get "human" in
  new character name password r
