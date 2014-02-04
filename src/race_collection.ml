(*
 Copyright 2014, 2004, 2003 Sean Proctor, Mike MacHenry

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

(* template_collection.ml: contains the list of templates *)
open Types

class race_collection =
  object (self)
    val mutable races : (string * race) list = []
    method add id r =
      races <- (id, r)::races
    method get (id : string) : race =
      try
        List.assoc id races
      with Not_found -> raise (Failure "race_collection#get")
  end

let races = new race_collection
