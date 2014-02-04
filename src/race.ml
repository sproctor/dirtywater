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

(* race.ml: a race generator *)

open Helpers
open Types
open Creature

class bodypart_def (bp : bodypart_type) =
  object (self)
    val my_type = bp

    method create (c : container) : bodypart =
      new bodypart my_type c

  end

class hand_def (s : side) =
  object (self)
    inherit bodypart_def (Hand s) as super

    method create (c : container) : bodypart =
      new hand s c

  end

class humanoid_race n =
  object (self)
    inherit race

    val mutable body_def : bodypart_def list =
      [
        new bodypart_def Head;
        new bodypart_def Torso;
        new bodypart_def (Arm Left);
        new bodypart_def (Arm Right);
        new hand_def Left;
        new hand_def Right;
        new bodypart_def (Leg Left);
        new bodypart_def (Leg Right);
        new bodypart_def (Foot Left);
        new bodypart_def (Foot Right);
      ]
    val name = n

    method create_creature (character_name : string) (con : container)
        : creature =
      let cr = new base_creature character_name con in
      let add_part bp_def =
        let bp = bp_def#create (cr :> container) in
        cr#add_bodypart bp in
      List.iter add_part body_def;
      (cr :> creature)

    method get_name = name

  end
