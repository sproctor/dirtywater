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

(* server.ml : This file is responcible for providing a server 
   that accepts connections to a given port. When connections are
   found, a connection object is created and added to the global list, then
   the login sequence is initiated. Successful logins
   get controllers and are set to control a character in the game. *)

open Types
open Helpers
open Base

(* max number of pending connections *)
let max_pending = 3

(* logs a user in and creates a controller of a character with the chanels *)
let establish_connection ((sock : Unix.file_descr), (caller : Unix.sockaddr)) =
  let conn = new Connection.telnet_connection sock in
  Connections.add (conn :> connection)

(* start up the server to accept connections *)
let start_server port =
  let sockaddr = (Unix.ADDR_INET(Unix.inet_addr_any, port)) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.setsockopt_optint sock Unix.SO_LINGER None;
  (try Unix.bind sock sockaddr with _ -> (dlog 0 "couldn't bind to port";
    exit 1));
  Unix.listen sock max_pending;
  dlog 0 ("Starting connection server on port " ^ string_of_int port);
  sock

(* checks which connections have input, then creates new connections for the
   ones on sock, and processes the appropriate input *)
let iterate_server sock delay =
  let (in_descriptors, _, _) =
    Unix.select (sock :: (Connections.get_descriptors ())) [] [] delay in
  if List.exists ((=) sock) in_descriptors then begin
    establish_connection (Unix.accept sock);
    dlog 4 "got a new connection"
  end;
  Connections.do_input in_descriptors

(* disconnects all connections and stops new connections *)
let disconnect sock =
  Connections.disconnect_all ();
  Unix.close sock;
  dlog 0 "Stopping the connection server"
