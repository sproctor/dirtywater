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

(*
   load.ml: this file contains the generic function to load the data files
*)

open Unix

let rec load_generic prefix load_function =
  let dir = opendir prefix in
  let rec get_files () =
    try
      let n = readdir dir in
      let name = Filename.concat prefix n in
      name :: (get_files ())
    with End_of_file -> [] in
  let filenames = get_files () in
  let filter_name str =
    ((stat str).st_kind <> S_DIR)
      && (Filename.check_suffix str ".yaml") in
  List.iter (function str -> load_function str)
    (List.filter filter_name filenames)
