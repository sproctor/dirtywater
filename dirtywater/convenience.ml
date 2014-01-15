(*
 Copyright 2014 Sean Proctor, Mike MacHenry

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

(* convenience.ml: convenience functions for use in world builer code.
     Not for use in general code *)

open Types
open Location
open Location_collection
open Template_collection

let create_room (loc_id : int) (title : string) (desc : string)
    (directions : (direction * int) list) =
  let loc = new simple_location loc_id title desc in
  let add_dir (dir, id) =
    loc#add_portal (new direction_portal dir id (loc :> container)) in
  List.iter add_dir directions;
  locations#add loc_id loc;
  loc

let place_tangible (id : string) (target : container) (where : position)
    : tangible =
  templates#create_tangible id target where
