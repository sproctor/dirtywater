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

open Types
open Helpers
open Base
open Debug

(* an object that can contain things *)
class virtual container =
  object (self)
    inherit iContainer
    inherit mud_object
    val mutable contents : (preposition * iTangible) list = []
    method can_add (prep : preposition) (thing : iTangible) =
      not (List.exists ((==) thing) (self#get prep))
    method add (prep : preposition) (thing : iTangible) : unit =
      if List.exists ((==) thing) (self#get prep) then
        raise (Failure "container#add")
      else contents <- (prep, thing)::contents
    method can_remove (thing : iTangible) = true
    method remove (thing : iTangible) : unit =
      let len = List.length contents in
      contents <- List.filter (function (_, t) -> t != thing) contents;
      if len - 1 <> List.length contents then raise (Failure "container#remove")
    method get (prep : preposition) : iTangible list =
      List.map (function (_, t) -> t)
        (List.filter self#preposition_matches contents)
    method contains (prep : preposition) (thing : iTangible) : bool =
      List.exists ((==) thing) (self#get prep)
    method private preposition_matches
      ((prep : preposition), (thing : iTangible)) : bool =
      try
        let (p, _) = List.find (function (_, t) -> t == thing) contents in
        prep = p
      with Not_found -> false
    method find looker (desc : object_desc) (count : int) =
      let rec find_in_list ((ord, adjs, noun) as nd) count things =
        match things with
            x::xs ->
              if (x#can_be_found looker)
                  && (x#matches_description adjs noun) then (
                match ord with
                  | Some num -> if num - count = 1 then x
                      else find_in_list nd (count + 1) xs
                  | None -> x
              ) else find_in_list nd count xs
          | [] -> raise (Object_not_found ((ObjectDescBase nd), count))
      in
      let rec search_in_list (l : iTangible list) (count : int) =
        match l with
          | x::xs -> (try x#find looker desc count
              with Object_not_found (desc, num) -> search_in_list xs num)
          | [] -> raise (Object_not_found (desc, count))
      in
      match desc with
          ObjectDescBase nd -> (try find_in_list nd count (self#get Anywhere)
              with Object_not_found (_, n) -> search_in_list (self#get Anywhere)
                n)
        (* next condition shouldn't happen with non-locations *)
        | ObjectDescRelative (od, p) -> search_in_list (self#get p) count
        | ObjectDesc _ -> search_in_list (self#get Anywhere) count
   end
