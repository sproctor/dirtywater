(*
 Copyright 2004, 2003 Sean Proctor, Mike MacHenry

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

(* tangible_collection.ml: contains a list of all tangibles in the game *)

open Types

class tangible_collection =
  object (self)
    val mutable tangibles : (int * iTangible) list = []
    val mutable next = 0
    method add id tangible =
      tangibles <- (id, tangible)::tangibles;
      if id >= next then next <- id + 1
    method get id =
      List.assoc id tangibles
    method get_id =
      let n = next in
      next <- next + 1;
      n
  end
