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
open Debug
open Container
open State

(* a physical object used to represent the physical exits from locations *)
class direction_object (d : direction) (dst : int) =
  let name = List.assoc d direction_list in
  object
    inherit tangible (tangibles#get_id) [] name name name
    val dir = d
    val dest_id = dst
    method get_short_desc (looker : iCreature) = MudString "error!!"
    method get_long_desc (looker : iCreature) =
      (locations#get dest_id)#get_description looker
    method matches_description adjs name =
      try
        (adjs = []) && ((direction_of_string name) = dir)
      with Not_found -> false
    method is_visible (looker : iCreature) = true
  end

(* an area in the world *)
class location (i : int) (t : string) (d : string) (ps : iPortal list) =
  object (self)
    inherit iLocation
    inherit container as super
    val id = i
    val width = 10.0
    val depth = 10.0
    val title = t
    val desc = d
    val mutable portals : iPortal list = ps

    method get_contents (con : containment option) : iTangible list =
      (super#get_contents con)@(List.map (function p -> p#tangible) portals)

    method relay_message (msg: mud_string) : unit =
      let creatures = map_some (function o -> o#as_creature)
          (self#get_contents None) in
      List.iter (function o -> o#send_message msg) creatures

    method get_description (looker : iCreature) =
      let objs = List.filter (fun t -> not (List.exists
            (fun p -> p#tangible = t) portals))
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

    method add_portal (p : iPortal) =
      portals <- p::portals

    method get_exit (e : exit) : iPortal option =
      let rec find_portal pl =
        match pl with
	    p::ps -> if p#has_exit e then Some p
	      else find_portal ps
	  | []    -> None
      in find_portal portals

    method get_location : iLocation = (self : #iLocation :> iLocation)

    initializer
      locations#add id (self : #iLocation :> iLocation)
  end

(* a way to get from one location to another. this can be a directions such
   as "north" or it can be associated with a tangible object, like a door *)
class portal (d : direction option) (o : iTangible) (d_id : int) =
  object (self)
    inherit iPortal
    val dest_id = d_id
    val obj = o
    val dir = d
    method can_pass (thing : iTangible) : bool = true
    method has_exit (e : exit) : bool =
      match e with
          ExitDir d -> (dir = Some d)
        | ExitObj o -> (obj == o)
    method dest = locations#get dest_id
    method tangible = o
  end

let create_portal (d : direction) (d_id : int) =
  new portal (Some d) (new direction_object d d_id) d_id
