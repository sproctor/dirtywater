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
   load_locations.ml: this file contains the functions to load the locations
     of the world.
*)

open Pxp_yacc
open Pxp_document

open Helpers
open Debug
open Types
open Base
open State
open Load
open Location

class default_location_ext =
  object (self)

    inherit [default_location_ext] generic_default_ext

    method get_location : iLocation = assert false
    method get_title : string option = None
    method get_description : string option = None
    method make_portals (l : iLocation) : iPortal list = assert false
    method get_portal (l : iLocation) : iPortal option = None
    method add_tangible (l : iLocation) : unit = ()
  end

class location_ext =
  object (self)

    inherit default_location_ext

    method get_location =
      let n = get_opt node in
      let children = n#sub_nodes in
      let title = find_some (List.map (function o -> o#extension#get_title)
            children) in
      let desc = find_some (List.map (function o -> o#extension#get_description)
            children) in
      let location = new location
          (int_of_string (n#required_string_attribute "id")) title desc 10.0
          10.0 in
      List.iter (function o -> o#extension#add_tangible location) children;
      location

    method make_portals l =
      map_some (function o -> o#extension#get_portal l) (get_opt node)#sub_nodes
  end

class title_ext =
  object (self)

    inherit default_location_ext

    method get_title = Some (get_opt node)#data
  end

class description_ext =
  object (self)

    inherit default_location_ext

    method get_description = Some (get_opt node)#data
  end

class portal_ext =
  object (self)

    inherit default_location_ext

    method get_portal l =
      let n = get_opt node in
      let dest = locations#get (int_of_string
            (n#required_string_attribute "destination")) in
      let (dir, obj) =
        match n#optional_string_attribute "direction" with
          | Some d ->
              let dir = direction_of_string d in
              let thing = new direction_object dir l dest in
              l#add Anywhere thing;
              (Some dir, thing)
          | None -> assert false in
      dlog 3 "creating portal";
      Some (new portal dir obj l dest)
  end

class location_tangible_ext =
  object (self)

    inherit default_location_ext

    method add_tangible l =
      let thing = tangibles#get (int_of_string
            ((get_opt node)#required_string_attribute "id")) in
      thing#move_to [((l :> iContainer), Anywhere)]
  end

let location_spec = make_spec_from_alist
    ~data_exemplar:             (new data_impl (new default_location_ext))
    ~default_element_exemplar:  (new element_impl (new default_location_ext))
    ~element_alist:             [
        "location",     new element_impl (new location_ext);
        "title",        new element_impl (new title_ext);
        "description",  new element_impl (new description_ext);
        "portal",       new element_impl (new portal_ext);
        "tangible",     new element_impl (new location_tangible_ext);
      ]
    ()

let load_locations () =
  let create_location (str: string) =
    let d = parse_document_entity default_config (from_file str) location_spec
      in
    (d, d#root#extension#get_location) in
  let make_portals (doc: default_location_ext document) (source: iLocation) =
    ignore (doc#root#extension#make_portals source) in
  let locations = load_generic "data/state/locations" create_location in
  List.iter (function (d, l) -> make_portals d l) locations
