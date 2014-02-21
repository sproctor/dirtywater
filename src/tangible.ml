(*
 Copyright 2014, 2003 Sean Proctor, Mike MacHenry

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

(* tangible.ml: this file contains the tangible class. a tangible is any
   object in the mud than can be manipulated in some way.
*)

open Types
open Helpers
open Container
open Base

(* base physical object *)
class virtual base_tangible =
  object (self)
    inherit tangible

    method get_location : location = self#get_parent#get_location

    method move (actor : creature) (dest : container) (where : position)
        : unit =
      (** First check that we can do this **)
      (* if can't remove from parent abort *)
      if not ((self#get_parent)#can_remove actor (self : #tangible :> tangible))
        then raise (Cannot_remove (self : #tangible :> tangible))
      (* if can't add to the new parent abort *)
      else if not (dest#can_add actor (self : #tangible :> tangible) where) then
        raise (Cannot_add (self : #tangible :> tangible))
      else begin
        (** then do it **)
        (* remove from old parent *)
        (self#get_parent)#remove (self : #tangible :> tangible);
        (* add to new parent *)
        dest#add (self : #tangible :> tangible) where;
        (* make the new parent the current parent *)
        self#set_parent dest
      end
  end

class actual_tangible (a : string list) (n : string) (sd : string)
    (ld : string) (p : container) (cs : position list) =
  object (self)
    inherit base_tangible
    inherit simple_container

    val name = n
    val adjs = a
    val short_desc = sd
    val long_desc = ld
    val mutable parent : container = p
    val containers = cs

    method private set_parent p = parent <- p

    method get_parent : container =
      parent

    method matches_description sadjs sname =
      if not (starts_with name sname) then false
      else not (List.exists (function sadj -> List.exists
            (function adj -> not (starts_with adj sadj)) adjs) sadjs)

    method can_be_gotten looker = true

    method is_visible looker = true

    method is_shown looker = true

    method get_name = name

    method short_description looker =
      let contents_on = self # view_contents looker (Some On) in
      let contents_in = self # view_contents looker (Some In) in
      if contents_on = [] && contents_in = [] then
        Mudstring short_desc
      else
        let get_desc c s =
          if c = [] then
            Mudstring_none
          else
            Mudstring_list (Separator_space, [
              Mudstring_list (Separator_comma, List.map (fun o -> o # short_description looker) c);
              Mudstring s
            ]) in
        Mudstring_list (Separator_space,
          [
            Mudstring short_desc;
            Mudstring "with";
            get_desc contents_in "in it";
            get_desc contents_on "on it";
          ])

    method look_description looker =
      Mudstring long_desc

    method to_string : string =
      "tangible: " ^ short_desc

    method send_message msg = ()

    method look_position_description looker where =
      if not (List.mem where containers) then
        Mudstring ("The " ^ self#get_name ^ " cannot contain anything there.")
      else
        let things = self#view_contents looker (Some where) in
        let descs = List.map (fun t -> t#short_description looker) things in
        Mudstring_list (Separator_newline,
            [Mudstring ("On the " ^ self#get_name ^ " you see:");
              Mudstring_list (Separator_comma, descs)])

  end

(* FIXME: need a better name for this *)
class virtual intangible =
  object
    inherit tangible
    method move cr con pos = raise (Failure "intangible#move")
    method can_be_gotten cr = false
    method send_message msg = ()
  end
