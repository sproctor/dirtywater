(*
 Copyright 2003, 2004 Sean Proctor, Mike MacHenry

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
open Location
open Tangible
open Building

let start_room = new location "Starting Room" "...with a simple description"
    10.0 10.0

let _ =
  let other_room = new location "Other Room"
    "This room is inferior to the starting room" 8.0 8.0 in
  let third_room = new location "Third Room"
    "This is a dark, mysterious room, you can't tell much about it." 9.0 9.0 in
  let cottage_room = new location "Inside the Cottage"
    ("The air is slightly musty. Everything is coated in a think layer of dust,"
    ^ " giving the appear that the cottage has been abandoned for a number of"
    ^ " years.") 10.0 10.0 in
  let door = make_tangible_in_room ~adjs:["small"; "wooden"] ~name:"door"
    ~short_desc:"a small wooden door leads outside" cottage_room in
  let cottage = make_building_in_room ~adjs:["small"] ~name:"cottage"
    ~short_desc:"a small cottage on a slight hill" start_room in
  let second_cottage = make_building_in_room ~adjs:["tiny"] ~name:"cottage"
    ~short_desc:"a tiny cottage with a collapsing roof" start_room in
  let rock = make_tangible_in_room ~name:"rock" start_room in
  let other_rock = make_tangible_in_room ~adjs:["gray"] ~name:"rock"
    start_room in
  ignore (make_portal (ExitDir North) start_room other_room);
  ignore (make_portal (ExitDir South) other_room start_room);
  ignore (make_portal (ExitDir NorthEast) other_room third_room);
  ignore (make_portal (ExitDir SouthWest) third_room other_room);
  ignore (make_portal (ExitObj cottage) start_room cottage_room);
  ignore (make_portal (ExitObj door) cottage_room start_room)
