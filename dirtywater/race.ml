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

(* template.ml: a template to create new tangibles *)

open Helpers
open Types
open State
open Creature
open Character

class bodypartDef (bp : bodypart_type) (r_parts : bodypartDef list) =
  object (self)
    val receive_list : bodypartDef list = r_parts
    val my_type = bp

    method create : bodypart =
      new bodypart my_type (List.map (function b -> b#create) receive_list)
  end

class race id n b =
  object (self)

    inherit iRace

    val mutable body : bodypartDef = b
    val name = n

    method create (id : int) (name : string) (password : string) =
      (* FIXME: this function needs to be expanded when we have more than
         just characters... not sure how yet *)
      new character id name password (body#create)

    initializer
      races#add id (self : #iRace :> iRace)
  end

let make_character name password start =
  let r = races#get "normalhuman" in
  let ch = r#create (tangibles#get_id) name password in
  ch#move_to [((start :> iContainer), On)];
  ch
