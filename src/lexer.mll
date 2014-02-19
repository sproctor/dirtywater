{
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

(*
   lexer.mll: this file contains the methods for lexing input received from
   the players

   a note: we should have lexers with state. if we receive certain sequences
   of token, we might want to have another condition on lexing. for example:
   '<box on table> shazam
   might need a special lexer for "box on table"
*)

open Types
open Helpers
open Parser
open Lexing
open Base
}

let whitespace = [' ''\t''\r''\n']

rule default_lexer =
  parse
  | whitespace  { default_lexer lexbuf }
  | "the"       { dlog 0 "article"; ARTICLE }
  | "an"        { dlog 0 "article"; ARTICLE }
  | "a"         { dlog 0 "article"; ARTICLE }
  | "on"        { dlog 0 "on"; ON }
  | "in"        { dlog 0 "in"; IN }
  | "from"	{ dlog 0 "from"; FROM }
  | "under"     { dlog 0 "under"; UNDER }
  | "behind"	{ dlog 0 "behind"; BEHIND }
  | "with"      { dlog 0 "with"; WITH }
  | "w/"        { dlog 0 "with"; WITH }
  | "and"       { dlog 0 "and"; AND }
  | (['0'-'9']+ as n)   { dlog 0 ("number " ^ n); NUMBER (int_of_string n) }
  | (['0'-'9']+ as n)("st"|"nd"|"rd"|"th") { dlog 0 ("ordinal " ^ n); ORDINAL (int_of_string n) }
      | (['a'-'z''\'']+ as w)       { dlog 0 ("word " ^ w); WORD w }
      | eof         { dlog 0 "eof"; EOF }
and say_lexer =
  parse '!'(['a'-'z''A'-'Z']+ as e)         { dlog 0 "emote"; EMOTE (string_to_emote e) }
      | '>'(['a'-'z''A'-'Z']+ as t)         { dlog 0 "target"; TARGET t }
      | (([^'!''>'' ''\t''\r''\n']_+) as w) { dlog 0 ("word " ^ w); WORD w }
      | whitespace                          { say_lexer lexbuf }
      | eof                                 { dlog 0 "eof"; EOF }
{
let cmd_list = [
  ("'", (Parser.say, say_lexer));
  ("\"", (Parser.say, say_lexer));
  ("attack", (Parser.attack, default_lexer));
  ("down", (Parser.down, default_lexer));
  ("drop", (Parser.drop, default_lexer));
  ("east", (Parser.east, default_lexer));
  ("go", (Parser.go, default_lexer));
  ("get", (Parser.take, default_lexer));
  ("inventory", (Parser.inventory, default_lexer));
  ("look", (Parser.look, default_lexer));
  ("north", (Parser.north, default_lexer));
  ("northeast", (Parser.northeast, default_lexer));
  ("northwest", (Parser.northwest, default_lexer));
  ("ne", (Parser.northeast, default_lexer));
  ("nw", (Parser.northwest, default_lexer));
  ("quit", (Parser.quit, default_lexer));
  ("south", (Parser.south, default_lexer));
  ("say", (Parser.say, say_lexer));
  ("southeast", (Parser.southeast, default_lexer));
  ("southwest", (Parser.southwest, default_lexer));
  ("se", (Parser.southeast, default_lexer));
  ("sw", (Parser.southwest, default_lexer));
  ("take", (Parser.take, default_lexer));
  ("up", (Parser.up, default_lexer));
  ("west", (Parser.west, default_lexer));
  ("wait", (Parser.wait, default_lexer))]

let parse_command str =
  let reg = Str.regexp "[\t\r\n ]*\\([A-Za-z]+\\|['\"]\\)\\(.*\\)" in
  if not (Str.string_match reg str 0) then raise (Bad_command "Bad input");
  let s = try
      Str.matched_group 1 str
    with Not_found -> raise (Bad_command "No command given.") in
  dlog 0 ("first match " ^ s);
  let (handler, lexer) = try
    assoc_fun (start_of s) cmd_list
  with Not_found -> raise (Bad_command "I don't understand that.") in
  let remainder = Str.matched_group 2 str in
  let lexbuf = Lexing.from_string remainder in
  dlog 0 ("remainder " ^ remainder);
  dlog 4 "created the lexbuf";
  try handler lexer lexbuf
  with Parsing.Parse_error ->
      raise (Bad_command "I don't understand what you mean.")
}
