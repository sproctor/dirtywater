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
   load_template.ml: this file contains the functions to load the templates
     to create tangibles
*)

open Pxp_yacc
open Pxp_document

open Helpers
open Debug
open Types
open Load
open Template

class default_template_ext =
  object

    inherit [default_template_ext] generic_default_ext

    method create_template : iTemplate = assert false

    method add_adj (t : iTemplate) : unit = ()

    method set_name (t : iTemplate) : unit = ()

    method set_sdesc (t : iTemplate) : unit = ()

    method set_ldesc (t : iTemplate) : unit = ()
  end

class template_ext =
  object (self)

    inherit default_template_ext

    method create_template : iTemplate =
      let n = get_opt node in
      let children = n#sub_nodes in
      let id = int_of_string (n#required_string_attribute "id") in
      let t = new template id in
      List.iter (function n -> n#extension#add_adj t) children;
      List.iter (function n -> n#extension#set_name t) children;
      List.iter (function n -> n#extension#set_sdesc t) children;
      List.iter (function n -> n#extension#set_ldesc t) children;
      t
  end

class adjective_ext =
  object (self)

    inherit default_template_ext

    method add_adj t = t#add_adj (get_opt node)#data
  end

class name_ext =
  object (self)

    inherit default_template_ext

    method set_name t = t#set_name (get_opt node)#data
  end

class short_desc_ext =
  object (self)

    inherit default_template_ext

    method set_sdesc t = t#set_sdesc (get_opt node)#data
  end

class long_desc_ext =
  object (self)

    inherit default_template_ext

    method set_ldesc t = t#set_ldesc (get_opt node)#data
  end

let template_spec = make_spec_from_alist
    ~data_exemplar:             (new data_impl (new default_template_ext))
    ~default_element_exemplar:  (new element_impl (new default_template_ext))
    ~element_alist:             [
        "template",     new element_impl (new template_ext);
        "adjective",    new element_impl (new adjective_ext);
        "name",         new element_impl (new name_ext);
        "sdesc",        new element_impl (new short_desc_ext);
        "ldesc",        new element_impl (new long_desc_ext);
      ]
    ()

let load_templates () =
  let create_template (str: string) =
    dlog 0 ("filename: " ^ str ^ "\n");
    let d = parse_document_entity default_config (from_file str) template_spec
      in
    (d, d#root#extension#create_template) in
  ignore (load_generic "data/tangibles" create_template)
