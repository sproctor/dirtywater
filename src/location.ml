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

(*
   location.ml: this file contains the definition for the location class.
   locations are rooms, etc. they are where objects exist. the base containers.
   portals are connections between locations.
*)

open Types
open Base
open Tangible
open Helpers
open Container
open Location_collection

(* a physical object used to represent the physical exits from locations *)
class direction_object (d : direction) (dst : int) (p : container) =
  object
    inherit tangible
    inherit noncontainer
    inherit intangible

    val dir = d
    val dest_id = dst
    val parent = p

    method private set_parent = raise (Failure "direction_object#set_parent")

    method get_parent = parent

    method get_name = List.assoc dir direction_list

    method get_location = parent#get_location

    method to_string = "direction_object: " ^ (List.assoc dir direction_list)
        ^ ", id: " ^ (string_of_int dest_id)

    method short_description (looker : creature) = MudString "error!!"

    method look_description (looker : creature) =
      (locations#get dest_id)#look_description looker

    method look_position_description looker where =
      MudString ("You cannot look \"" ^ (string_of_position where) ^"\" that.")

    method matches_description adjs name =
      try
        (adjs = []) && ((direction_of_string name) = dir)
      with Not_found -> false

    method is_visible (looker : creature) = true

    method is_shown (looker : creature) = false
  end

(* an area in the world *)
class simple_location (i : int) (t : string) (d : string) =
  object (self)
    inherit location
    inherit simple_container as super

    val id = i
    val width = 10.0
    val depth = 10.0
    val title = t
    val desc = d
    val mutable portals : portal list = []

    method to_string =
      "location " ^ (string_of_int id) ^ ": " ^ title

    method get_contents (where : position option) : tangible list =
      let portal_objects = List.map (function p -> p#tangible) portals in
      let other_objects = super#get_contents where in
      match where with
      | Some In -> other_objects@portal_objects
      | None -> other_objects@portal_objects
      | _ -> other_objects

    method relay_message (msg: mud_string) : unit =
      List.iter (function o -> o#send_message msg) (self#get_contents None)

    method get_description (looker : creature) =
      let objs = List.filter (fun t -> t#is_shown looker)
          (self#view_contents looker None) in
      let exits = List.map (fun (_, str) -> MudString str)
        (List.filter (fun (dir, _) -> self#get_exit (ExitDir dir) <> None)
        direction_list) in
      MudStringList (SeparatorNewline,
        [MudStringMeta (MetaRoomTitle, MudString title);
          MudStringMeta (MetaRoomDesc, MudString desc);
          if objs <> [] then MudStringMeta (MetaRoomContents, MudStringList
              (SeparatorComma, (List.map (function o -> MudStringName o)
                objs)))
            else MudStringNone;
          if exits <> [] then MudStringMeta (MetaRoomExits,
              MudStringList (SeparatorComma, exits))
            else MudString "none"])

    method add_portal (p : portal) =
      portals <- p::portals

    method get_exit (e : exit) : portal option =
      let rec find_portal pl =
        match pl with
	    p::ps -> if p#has_exit e then Some p
	      else find_portal ps
	  | []    -> None
      in find_portal portals

    method get_location : location = (self : #location :> location)

    method look_description looker = self#get_description looker

    method set_init (script : script) : unit =
      Script.run_location_script (self : #location :> location) script

    initializer
      locations#add id (self : #location :> location)
  end

(* a way to get from one location to another. this can be a directions such
   as "north" or it can be associated with a tangible object, like a door *)
class virtual simple_portal (id : int) =
  object (self)
    inherit portal

    val dest_id = id

    method dest = locations#get dest_id
    method can_pass (thing : tangible) : bool = true
  end

class direction_portal (d : direction) (d_id : int) (p : container) =
  object (self)
    inherit simple_portal d_id

    val obj = new direction_object d d_id p
    val dir = d

    method tangible = obj

    method has_exit (e : exit) : bool =
      match e with
        | ExitDir d -> (dir = d)
        | ExitObj o -> (obj == o)
  end

(* TODO: implement this when we need doors
class object_portal (o : tangible) (d_id : int) =
  object (self)
    inherit simple_portal d_id

    val obj = 
    method has_exit (e : exit) : bool =
      match e with
      | ExitDir _ -> false
      | ExitObj o -> (obj == o)
  end*)