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

(* types.ml : this file is responsible for providing all of the data types
   used by the mud
*)

type noun_desc = (int option * string list * string)

type preposition =
  | Under
  | On
  | In
  | From
  | Of
  | Between of (noun_desc * noun_desc)
  | Near of noun_desc
  | Anywhere

type object_desc =
  | ObjectDesc of (object_desc * preposition * noun_desc)
  | ObjectDescRelative of (object_desc * preposition)
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

type 'iTangible exit' =
    ExitDir of direction
  | ExitObj of 'iTangible

type exit_desc =
    ExitDescDir of direction
  | ExitDescObj of object_desc

type ('iMud_object, 'iContainer, 'iTangible, 'iCreature, 'iCharacter,
    'iLocation, 'iPortal) object_type' =
  | MudObject of 'iMud_object
  | ContainerObject of 'iContainer
  | TangibleObject of 'iTangible
  | CreatureObject of 'iCreature
  | CharacterObject of 'iCharacter
  | LocationObject of 'iLocation
  | PortalObject of 'iPortal
  | PrepositionObject of (preposition * ('iMud_object, 'iContainer, 'iTangible,
      'iCreature, 'iCharacter, 'iLocation, 'iPortal) object_type')

type emote =
  | EmoteQuietly
  | EmoteLoudly

(* player wait is the result of parsing the input string a player sends,
   it needs to be turned into a real command before being sent to the
   controller. there will be some time between when it's parsed and when this
   happens though *)
type player_command =
    Player_wait of int option
  | Player_attack of (object_desc option * object_desc option)
  | Player_move of exit_desc
  | Player_quit
  | Player_look of (preposition option * object_desc) option
  | Player_take of object_desc
  | Player_drop of object_desc
  | Player_inventory
  | Player_say of (emote list * string list* string)

(* command is the structure sent to the controller *)
type ('iMud_object, 'iContainer, 'iTangible, 'iCreature, 'iCharacter,
    'iLocation, 'iPortal) command' =
  | Cmd_wait of int
  | Cmd_attack of ('iTangible * 'iTangible)
  | Cmd_move of 'iPortal
  | Cmd_look of ('iMud_object, 'iContainer, 'iTangible, 'iCreature, 'iCharacter,
      'iLocation, 'iPortal) object_type'
  | Cmd_take of 'iTangible
  | Cmd_drop of 'iTangible
  | Cmd_inventory
  | Cmd_say of (emote list * 'iCharacter list * string)

type 'iCreature mud_meta' =
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

type ('iTangible, 'iCreature) mud_string' =
  | MudStringNone
  | MudString of string
  | MudStringMode of (graphics_mode * ('iTangible, 'iCreature) mud_string')
  | MudStringMeta of ('iCreature mud_meta'
      * ('iTangible, 'iCreature) mud_string')
  | MudStringList of (separator_type
        * ('iTangible, 'iCreature) mud_string' list)
  | MudStringCondition of ('iCreature * ('iTangible, 'iCreature) mud_string'
      * ('iTangible, 'iCreature) mud_string')
  | MudStringName of 'iTangible

type position = (float * float)

type side = Left | Right

type body_part_kind =
    Head
  | Leg of side
  | Arm of side
  | Hand of side
  | Foot of side
  | Torso

type 'iTangible body_part' = {
  kind : body_part_kind; 
  mutable attached_to : 'iTangible body_part' option;
  mutable receive_list : (body_part_kind * 'iTangible body_part') list;
  thing : 'iTangible;
}

type 'iTangible inventory' = (body_part_kind * preposition * 'iTangible list)
  list

class virtual iMud_object =
  object
    method virtual as_creature : iCreature option
  end
and virtual iController =
  object
    method virtual send_message : (iTangible, iCreature) mud_string' -> unit
  end
and virtual iContainer =
  object
    inherit iMud_object
    method virtual get_location : iLocation
    method virtual can_add : preposition -> iTangible -> bool
    method virtual add : preposition -> iTangible -> unit
    method virtual can_remove : iTangible -> bool
    method virtual remove : iTangible -> unit
    method virtual get : preposition -> iTangible list
    method virtual contains : preposition -> iTangible -> bool
    method virtual get_contents : iCreature -> preposition -> iTangible list
  end
and virtual iTangible =
  object
    inherit iContainer
    method virtual remove_from : iContainer -> unit
    method virtual add_to : preposition -> iContainer -> unit
    (* move this iTangible into the given containers *)
    method virtual move_to : (iContainer * preposition) list -> unit
    (* get the name of this iTangible (is this useful?) *)
    method virtual get_name : string
    (* match this iTangible against the given noun description *)
    method virtual matches_description : string list -> string -> bool
    method virtual can_be_gotten : iCreature -> bool
    (* can the given iCreature find this iTangible *)
    method virtual can_be_found : iCreature -> bool
    (* Should this iCreature be shown in room descriptions *)
    method virtual is_visible : iCreature -> bool
    (* return a short description of this iTangible which is custom made for
       the given iCreature *)
    method virtual get_short_desc : iCreature ->
        (iTangible, iCreature) mud_string'
    method virtual get_long_desc : iCreature ->
        (iTangible, iCreature) mud_string'
    (* should we do things this way? *)
    method virtual get_containers : iContainer list
  end
and virtual iCreature =
  object
    inherit iTangible
    (* called by the controller to give the character commands *)
    method virtual run_command : (iMud_object, iContainer, iTangible, iCreature,
        iCharacter, iLocation, iPortal) command' -> unit
    (* method called by the world and other objects when some stimulus
       effects the character *)
    method virtual set_controller : iController -> unit
    method virtual take : iTangible -> unit
    method virtual drop : iTangible -> unit
    method virtual get_inventory : iCreature -> iTangible inventory'
    method virtual look_for : object_desc -> iTangible
    method virtual send_message : (iTangible, iCreature) mud_string' -> unit
  end
and virtual iCharacter =
  object
    inherit iCreature
    (* called by the login to check if the given password is right *)
    method virtual match_password : string -> bool
    method virtual set_password : string -> unit
  end
and virtual iPortal =
  object
    method virtual can_pass : iTangible -> bool
    method virtual has_exit : iTangible exit' -> bool
    method virtual dest : iLocation
  end
and virtual iLocation =
  object
    inherit iContainer
    method virtual relay_message : (iTangible, iCreature) mud_string' -> unit
    method virtual add_portal : iPortal -> unit
    method virtual add_by_coords : iTangible -> position -> unit
    method virtual get_exit : iTangible exit' -> iPortal option
    method virtual get_description : iCreature ->
        (iTangible, iCreature) mud_string'
  end

class virtual iTemplate =
  object
    method virtual add_adj : string -> unit
    method virtual set_name : string -> unit
    method virtual set_sdesc : string -> unit
    method virtual set_ldesc : string -> unit
    method virtual create : int -> iTangible
  end

class virtual iPlayer =
  object
    inherit iController
    method virtual run_command : unit -> unit
    method virtual enqueue_command : string -> unit
  end

class virtual iConnection =
  object
    method virtual get_descriptor : Unix.file_descr
    method virtual input : unit -> unit
    method virtual output : string -> unit
    method virtual close : unit -> unit
  end

type mud_string = (iTangible, iCreature) mud_string'
type object_type = (iMud_object, iContainer, iTangible, iCreature, iCharacter,
  iLocation, iPortal) object_type'
type command = (iMud_object, iContainer, iTangible, iCreature, iCharacter,
  iLocation, iPortal) command'
type exit = iTangible exit'
type inventory = iTangible inventory'
type body_part = iTangible body_part'

class virtual iRace =
  object
    method virtual set_body : body_part -> unit
    method virtual create : int -> string -> iCharacter
  end

exception Bad_command of string
exception Command_error of string
exception Quit
exception Object_not_found of (object_desc * int)
exception Direction_not_valid of direction
exception Object_not_exit of iTangible
exception Too_big of iTangible
exception No_space_for of iTangible
exception Cannot_add of iTangible
exception Cannot_remove of iTangible
