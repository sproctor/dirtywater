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
open Tangible

class building (a : string list) (n : string) (sd : string) (ld : string) =
  object (self)
    inherit tangible a n sd ld []
    method can_be_gotten looker = false
  end

let make_building_in_room ?(adjs = []) ~name ?short_desc ?long_desc room =
  let sd = (match short_desc with Some x -> x
    | None -> String.concat " " (adjs@[name])) in
  let ld = (match long_desc with Some x -> x | None -> sd) in
  let thing = new building adjs name sd ld in
  thing#move_to [((room :> iContainer), Anywhere)];
  thing
