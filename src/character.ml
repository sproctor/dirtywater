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

(* character.ml : This file contains the base class for character from which
   all other character (players and nonplayers alike) are dervived. The
   character base class provides the interaction with the controller and
   implementations for the difference actions that can be called on a
   character. subclass will inherit from character and change values of private
   skill fields to make races diverse

   Sean 1/12/14 - I'm not sure about this. Characters might just be for
     players. NPC might just be creatures.
 *)

open Types
open Base
open Helpers
open Creature
open Race

(* basic character class *)
class character (n : string) (p : string) (r : race) =
  object (self)

    val mutable password = p
    val mutable race = r
    val name = n

    method create_creature (con : container) =
      let cr = race#create_creature name con in
      dlog 4 ("character#create_creature adding " ^ cr#to_string ^ " to "
          ^ con#to_string);
      con#add (cr :> tangible) On;
      cr

    (* called by the login to check if the given password is right *)
    method match_password (guess : string) : bool = (guess = password)

    method set_password p =
      password <- p

  end
