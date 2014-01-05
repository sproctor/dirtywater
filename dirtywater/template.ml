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
open State
open Helpers

class template id =
  object (self)

    inherit iTemplate

    val mutable adjs : string list = []
    val mutable name : string option = None
    val mutable sdesc : string option = None
    val mutable ldesc : string option = None

    method add_adj (str : string) : unit =
      adjs <- adjs@[str]

    method set_name (str : string) : unit =
      assert (name = None);
      name <- Some str

    method set_sdesc (str : string) : unit =
      assert (sdesc = None);
      sdesc <- Some str

    method set_ldesc (str : string) : unit =
      assert (ldesc = None);
      ldesc <- Some str

    method create (id : int) : iTangible =
      new tangible id adjs (get_opt name) (get_opt sdesc) (get_opt ldesc) []

    initializer
      templates#add id (self : #iTemplate :> iTemplate)
  end
