{
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
open Parser
open Lexing

let cmd_list = [
  ("attack", ATTACK);
  ("down", DOWN);
  ("drop", DROP);
  ("east", EAST);
  ("go", GO);
  ("get", TAKE);
  ("inventory", INVENTORY);
  ("look", LOOK);
  ("north", NORTH);
  ("northeast", NORTHEAST);
  ("northwest", NORTHWEST);
  ("ne", NORTHEAST);
  ("nw", NORTHWEST);
  ("quit", QUIT);
  ("south", SOUTH);
  ("southeast", SOUTHEAST);
  ("southwest", SOUTHWEST);
  ("se", SOUTHEAST);
  ("sw", SOUTHWEST);
  ("take", TAKE);
  ("up", UP);
  ("west", WEST);
  ("wait", WAIT)]

(* returns the command token of a string or raise Bad_command *)
(* the first letter is capitalized so we only check for commands at the
   beginning of the line. if we have a command in the middle, we don't want
   to treat it as a command. ex: open south door
   here south is obviously not a command, but by itself "south" is a command *)
let get_cmd (s : string) : token =
  try assoc_fun (start_of s) cmd_list
  with Not_found -> raise (Bad_command "I didn't understand that.")
}

let whitespace = [' ''\t''\r''\n']

rule main = parse
    whitespace  { main lexbuf }
  | "the"       { ARTICLE }
  | "an"        { ARTICLE }
  | "a"         { ARTICLE }
  | "under"     { UNDER }
  | "on"        { ON }
  | "in"        { IN }
  | "between"   { BETWEEN }
  | "near"      { NEAR }
  | "by"        { NEAR }
  | "with"      { WITH }
  | "w/"        { WITH }
  | "and"       { AND }
  | (['0'-'9']+ as n)   { NUMBER (int_of_string n) }
  | (['0'-'9']+ as n)("st"|"nd"|"rd"|"th")      { ORDINAL (int_of_string n) }
  | (['a'-'z''\'']+ as w)       { WORD w }
  | eof         { EOF }
and command = parse
    whitespace  { command lexbuf }
  | (['a'-'z''\'''"']+ as c)    { get_cmd c }
  | eof         { EOF }
{
let lexer lexbuf =
  if lexbuf.lex_curr_pos = 0 then
    command lexbuf
  else
    main lexbuf

(* parses strings into command structure *)
let parse_command (str : string) : player_command =
  let lexbuf = Lexing.from_string (String.lowercase str) in
  try Parser.main lexer lexbuf
  with Parsing.Parse_error ->
      raise (Bad_command "I don't understand what you mean.")
}
