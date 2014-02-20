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

(* types.ml : this file is responsible for providing all of the data types
   used by the mud
*)

type noun_desc = (int option * string list * string)

type preposition =
  | Prep_on
  | Prep_in
  | Prep_behind
  | Prep_under
  | Prep_any

type ('tangible) position' =
  | In
  | On
  | Under of 'tangible
  | Behind of 'tangible

type object_desc =
  | ObjectDesc of (object_desc * preposition * noun_desc)
  | ObjectDescBase of noun_desc

(* the ways to get out of a room *)
type direction =
  | North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  | Up
  | Down

type 'tangible exit' =
    ExitDir of direction
  | ExitObj of 'tangible

type exit_desc =
    ExitDescDir of direction
  | ExitDescObj of object_desc

type emote =
  | EmoteQuietly
  | EmoteLoudly

(* player wait is the result of parsing the input string a player sends,
   it needs to be turned into a real command before being sent to the
   controller. there will be some time between when it's parsed and when this
   happens though *)
type player_command =
  | Player_wait of int option
  | Player_attack of (object_desc option * object_desc option)
  | Player_move of exit_desc
  | Player_quit
  | Player_look of (preposition * object_desc) option
  | Player_take of object_desc
  | Player_drop of object_desc
  | Player_inventory
  | Player_say of (emote list * string list* string)

(* command is the structure sent to the controller *)
type ('mud_object, 'tangible, 'creature, 'portal) command' =
  | Cmd_wait of int
  | Cmd_attack of ('tangible * 'tangible)
  | Cmd_move of 'portal
  | Cmd_look of 'mud_object
  | Cmd_look_position of ('tangible * 'tangible position')
  | Cmd_take of 'tangible
  | Cmd_drop of 'tangible
  | Cmd_inventory
  | Cmd_say of (emote list * 'creature list * string)

type 'creature mud_meta' =
  | MetaInit
  | MetaWaitDone of int
  | MetaPrompt
  | MetaRoomTitle
  | MetaRoomDesc
  | MetaRoomContents
  | MetaRoomExits

type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Normal

type graphics_mode =
  | Bold
  | Foreground of color
  | Background of color

type separator_type =
  | SeparatorNewline
  | SeparatorNone
  | SeparatorSpace
  | SeparatorComma
  | SeparatorDefault

type ('tangible, 'creature) mud_string' =
  | MudStringNone
  | MudString of string
  | MudStringMode of (graphics_mode * ('tangible, 'creature) mud_string')
  | MudStringMeta of ('creature mud_meta'
      * ('tangible, 'creature) mud_string')
  | MudStringList of (separator_type
        * ('tangible, 'creature) mud_string' list)
  | MudStringCondition of ('creature * ('tangible, 'creature) mud_string'
      * ('tangible, 'creature) mud_string')
  | MudStringName of 'tangible

type side = Left | Right

type bodypart_type =
  | Head
  | Leg of side
  | Arm of side
  | Hand of side
  | Foot of side
  | Torso

type 'tangible inventory' = (bodypart_type * 'tangible position'
      * 'tangible list)
    list

type script =
  | Script_string of string
  | Script_file of string

class virtual mud_object =
  object
    method virtual look_description : creature
      -> (tangible, creature) mud_string'
    method virtual get_location : location
    method virtual to_string : string
  end
and virtual controller =
  object
    method virtual send_message : (tangible, creature) mud_string' -> unit
  end
and virtual container =
  object
    inherit mud_object
    method virtual can_add : creature -> tangible -> tangible position' -> bool
    method virtual add : tangible -> tangible position' -> unit
    method virtual can_remove : creature -> tangible -> bool
    method virtual remove : tangible -> unit
    method virtual get_contents : tangible position' option -> tangible list
    method virtual view_contents : creature -> tangible position' option
      -> tangible list
  end
and virtual tangible =
  object
    inherit container
    method private virtual set_parent : container -> unit
    method virtual get_parent : container
    (* move this tangible into the given container *)
    method virtual move : creature -> container -> tangible position' -> unit
    (* get the name of this tangible (is this useful?) *)
    method virtual get_name : string
    (* match this tangible against the given noun description *)
    method virtual matches_description : string list -> string -> bool
    method virtual can_be_gotten : creature -> bool
    (* can the given creature find this tangible *)
    method virtual is_visible : creature -> bool
    (* Should this creature be shown in room descriptions *)
    method virtual is_shown : creature -> bool
    (* return a short description of this tangible which is custom made for
       the given creature *)
    method virtual short_description : creature
      -> (tangible, creature) mud_string'
    method virtual look_position_description : creature -> tangible position'
      -> (tangible, creature) mud_string'
    method virtual send_message : (tangible, creature) mud_string' -> unit
  end
and virtual creature =
  object
    inherit tangible
    (* called by the controller to give the character commands *)
    method virtual run_command : (mud_object, tangible, creature, portal)
        command' -> unit
    (* method called by the world and other objects when some stimulus
       effects the character *)
    method virtual set_controller : controller -> unit
    method virtual take : tangible -> unit
    method virtual drop : tangible -> unit
    method virtual get_inventory : creature -> tangible inventory'
  end
and virtual portal =
  object
    method virtual can_pass : tangible -> bool
    method virtual has_exit : tangible exit' -> bool
    method virtual dest : location
    method virtual tangible : tangible
  end
and virtual location =
  object
    inherit container
    method virtual relay_message : (tangible, creature) mud_string' -> unit
    method virtual add_portal : portal -> unit
    method virtual get_exit : tangible exit' -> portal option
    method virtual get_description : creature -> (tangible, creature) mud_string'
    method virtual set_init : script -> unit
  end

class virtual race =
  object
    method virtual create_creature : string -> container -> creature
    method virtual get_name : string
  end

class virtual tangible_template =
  object
    method virtual create_tangible : container -> tangible
  end

class virtual player =
  object
    inherit controller
    method virtual run_command : unit -> unit
    method virtual enqueue_command : string -> unit
  end

class virtual connection =
  object
    method virtual get_descriptor : Unix.file_descr
    method virtual input : unit -> unit
    method virtual output : string -> unit
    method virtual close : unit -> unit
  end

type mud_string = (tangible, creature) mud_string'
type command = (mud_object, tangible, creature, portal) command'
type exit = tangible exit'
type inventory = tangible inventory'
type position = tangible position'

exception Bad_command of string
exception Command_error of string
exception Quit
exception Object_not_found of (object_desc * int)
exception Direction_not_valid of direction
exception Object_not_exit of tangible
exception Too_big of tangible
exception No_space_for of tangible
exception Cannot_add of tangible
exception Cannot_remove of tangible
