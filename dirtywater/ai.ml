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

(*
   ai.ml : This file responcible for providing all of the artificial
   intelligence of the mud. Some AI is for enemies and will be separated
   into separate subclasses, e.g. there may be a separate goblin and
   troll AI. This file will also provide AI's for users in certain
   situations, e.g. an AI for sitting around the library and reading while
   the user is logged off or an AI to protect an area from intruders
*)

open Types
open Character
open Debug

class simple_ai (ch : character) =
  object (self)
    inherit iController
    val character = ch
    (* method called by the character when its controller needs to know what
       is happening to the character *)
    method send_message (msg: mud_string) : unit = ()
      (*let send_message (msg, _) =
        match msg with
            Msg_init -> character#run_command (Cmd_wait 1);
              dlog 4 "got hello"
          | Msg_wait_end n ->
              dlog 4 ("ai: got wait end: " ^ string_of_int n);
              dlog 4 "ai: --------about to run wait";
              character#run_command (Cmd_wait (n + 1));
              dlog 4 "ai: ------ran wait"
          | Msg_wait_start _ -> dlog 4 "ai: got a wait start"
          | _ -> ()
      in List.iter send_message msgs*)
  end
