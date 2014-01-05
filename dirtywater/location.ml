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
open Base
open State
open Tangible
open Helpers
open Debug
open Container

class direction_object (d : direction) (src : iLocation) (dst : iLocation) =
  let name = List.assoc d direction_list in
  object
    inherit tangible [] name name name []
    val dir = d
    val source = src
    val dest = dst
    method get_short_desc (looker : iCreature) = "error!!"
    method get_long_desc (looker : iCreature) =
      dest#get_description looker
    method matches_description adjs name =
      adjs = [] && (try assoc_fun (start_of name) direction_lookup_list = dir
        with Not_found -> false)
    method is_visible (looker : iCreature) = false
  end

class location (t : string) (des : string) (wid : float) (dep : float) =
  object (self)
    inherit iLocation
    inherit container
    val width = wid
    val depth = dep
    val title = t
    val descr = des
    val mutable contents : (position * iTangible) list = []
    val mutable portals : iPortal list = []
    method private get_coords (p : preposition) : position =
      (0.0, 0.0)
    method relay_messages (msgs : message list) : unit =
      List.iter (function o -> o#send_messages msgs) (self#get Anywhere)
    method get_description (looker : iCreature) =
      let rec list_exits exits =
        match exits with
	    (dir, str)::ls ->
	      if self#get_exit (ExitDir dir) <> None then str::(list_exits ls)
              else list_exits ls
	  | [] -> []
      in
      let objs = List.filter (fun obj -> obj#is_visible looker)
        (self#get Anywhere) in
      let strings = List.map (fun obj -> obj#get_short_desc looker) objs in
      let exits = list_exits direction_list in
      "[" ^ title ^ "]\n"
        ^ descr ^ "\n" ^
        (if strings <> [] then
          "You also see here: " ^ (add_commas strings) ^ "\n" else "") ^
        (if exits <> [] then "Obvious exits: " ^ (add_commas exits) else "")
    method can_add (p : preposition) (thing : iTangible) = true
    method add (l : preposition) (o : iTangible) : unit =
      contents <- (self#get_coords l, o)::contents;
      dlog 2 ("added to location: " ^ o#get_name)
    method add_by_coords (o : iTangible) (p : position) : unit =
      contents <- (p, o)::contents
    method can_remove (thing : iTangible) = true
    method remove (thing : iTangible) : unit =
      contents <- List.filter (fun (_, obj) -> obj <> thing) contents
    method get (p : preposition) : iTangible list =
      List.map (fun (_, obj) -> obj) (List.filter self#has_preposition contents)
    method private has_preposition (prep, thing) = true
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
  end

class portal (d : direction option) (o : iTangible) (slocation : iLocation)
    (dlocation : iLocation) =
  object (self)
    inherit iPortal
    val source_location = slocation
    val dest_location = dlocation
    val obj = o
    val dir = d
    method can_pass (thing : iTangible) : bool = true
    method has_exit (e : exit) : bool =
      match e with
          ExitDir d -> (dir = Some d)
        | ExitObj o -> (obj == o)
    method dest = dest_location
    initializer source_location#add_portal (self : #iPortal :> iPortal)
  end

let make_portal e (source : iLocation) (dest : iLocation) =
  let (dir, obj) = match e with
      ExitDir d ->
        let thing = new direction_object d source dest in
        source#add Anywhere thing;
        Some d, thing
    | ExitObj o -> None, o in
  new portal dir obj source dest
