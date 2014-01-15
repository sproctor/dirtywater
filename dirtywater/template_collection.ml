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

(* template_collection.ml: contains the list of templates *)
open Types

class template_collection =
  object (self)
    val mutable templates : (string * tangible_template) list = []

    method add id template =
      templates <- (id, template)::templates

    method get (id : string) =
      List.assoc id templates

    method create_tangible (template_id : string) (con : container)
        (where : position) : tangible =
      let thing = (self#get template_id)#create_tangible con in
      con#add thing where;
      thing

  end

let templates = new template_collection
