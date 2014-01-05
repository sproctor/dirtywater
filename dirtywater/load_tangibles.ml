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
   load_tangibles.ml: this file contains the functions to load the state of the
     tangibles in the world
*)

open Pxp_yacc
open Pxp_document

open Helpers
open Debug
open Types
open State
open Load

class default_tangible_ext =
  object (self)

    inherit [default_tangible_ext] generic_default_ext

    method get_tangible : iTangible = assert false

    method get_id : int option = assert false
  end

class tangible_ext =
  object (self)

    inherit default_tangible_ext

    method get_tangible =
      let n = get_opt node in
      let children = n#sub_nodes in
      let template_id = find_some (List.map
            (function n -> n#extension#get_id) children) in
      let tangible_id = int_of_string (n#required_string_attribute "id") in
      templates#create template_id tangible_id
  end

class tangible_temp_ext =
  object (self)

    inherit default_tangible_ext

    method get_id : int option =
      Some (int_of_string ((get_opt node)#required_string_attribute "id"))
  end

let tangible_spec = make_spec_from_alist
    ~data_exemplar:             (new data_impl (new default_tangible_ext))
    ~default_element_exemplar:  (new element_impl (new default_tangible_ext))
    ~element_alist:             [
        "tangible",     new element_impl (new tangible_ext);
        "template",     new element_impl (new tangible_temp_ext);
      ]
    ()

let load_tangibles () =
  let create_tangible (str: string) =
    dlog 0 ("filename: " ^ str ^ "\n");
    let d = parse_document_entity default_config (from_file str) tangible_spec
      in
    (d, d#root#extension#get_tangible) in
  ignore (load_generic "data/state/tangibles" create_tangible)
