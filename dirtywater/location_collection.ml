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
   location_collection.ml: this file contains the class listing all the
   locations. any new location created should be added to this list. *)
open Types

class location_collection =
  object
    val mutable location_list : (int * iLocation) list = []
    method add (i: int) (r: iLocation) : unit =
      location_list <- (i, r)::location_list
    method get (i: int) : iLocation =
      List.assoc i location_list
  end
