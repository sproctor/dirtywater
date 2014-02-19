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
   connection_collection.ml: this file contains the class to store all of the
   connections. any methods that need to affect all connections should be
   added here. do_input processes one line of input for each connection
*)

open Types

let connections : connection list ref = ref []

let get_descriptors () =
  List.map (fun x -> x#get_descriptor) !connections

let add c =
  connections := c :: !connections

let remove c =
  connections := List.filter ((!=) c) !connections

let do_input descriptors =
  List.iter (fun x -> x#input ())
    (List.filter (fun x -> List.exists ((=) x#get_descriptor) descriptors) !connections)

let disconnect_all () = List.iter (fun x -> x#close ()) !connections
