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

(* template.ml: a template to create new tangibles *)

open Types
open Tangible
open Helpers

class simple_template (a : string list) (n : string) (s : string) (l : string) =
  object (self)
    inherit tangible_template

    val adjs : string list = a
    val name : string = n
    val sdesc : string = s
    val ldesc : string = l

    method create_tangible (con : container) : tangible =
      new simple_tangible adjs name sdesc ldesc con

  end

class container_template (a : string list) (n : string) (s : string) (l : string) =
  object (self)
    inherit tangible_template

    val adjs : string list = a
    val name : string = n
    val sdesc : string = s
    val ldesc : string = l

    method create_tangible (con : container) : tangible =
      new simple_tangible_container adjs name sdesc ldesc con

  end

class generic_tangible_template (f : container -> tangible) =
  object (self)
    inherit tangible_template

    val factory = f

    method create_tangible (con : container) : tangible =
      factory con
  end
