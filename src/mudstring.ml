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
   mud_string.ml: this file implements a custom string that is made of
   multiple parts. these strings can contain meta data and color data.
   Any string that may eventually be shown to a character should be a
   mud_string. any function dealing with strings that may be shown to characters
   should deal with mud_strings.
*)

open Types
open Helpers

(*let merge (sep: separator_type) (str1: mud_string) (str2: mud_string)
    : mud_string =
  match (str1, str2) with
    | (MudStringNone, s) -> s
    | (s, MudStringNone) -> s
    | (MudStringList (x, l1), MudStringList (y, l2)) ->
        if sep = SeparatorDefault || (x = sep && x = y)
          then MudStringList (x, (l1@l2))
          else MudStringList (sep, [str1; str2])
    | (MudStringList (x, l), s) ->
        if x = sep || sep = SeparatorDefault then MudStringList (x, l@[s])
        else MudStringList (sep, [str1; str2])
    | (s, MudStringList (x, l)) ->
        if x = sep || sep = SeparatorDefault then MudStringList (x, s::l)
        else MudStringList (sep, [str1; str2])
    | (s1, s2) -> MudStringList (sep, [s1; s2])

let rec concat (sep: separator_type) (strs: mud_string list) : mud_string =
  match strs with
    | [] -> MudStringNone
    | str::[] -> str
    | str1::str2::rest -> concat sep ((merge sep str1 str2)::rest)
*)
let to_string (looker: creature) (str: mud_string) : string =
  let rec to_string_helper (fg: color) (bg: color) (str: mud_string) : string =
    match str with
      | Mudstring_none -> ""
      | Mudstring s -> s
      | Mudstring_mode (code, s) ->
          let codify c = "\027[" ^ (string_of_int c) ^ "m" in
          let color_to_int color =
            match color with
              | Black   -> 0
              | Red     -> 1
              | Green   -> 2
              | Yellow  -> 3
              | Blue    -> 4
              | Magenta -> 5
              | Cyan    -> 6
              | White   -> 7
              | Normal  -> 9 in
          (match code with
            | Bold -> (codify 1) ^ (to_string_helper fg bg s) ^ (codify 22)
            | Foreground c -> (codify (color_to_int c + 30))
                ^ (to_string_helper c bg s) ^ (codify (color_to_int fg + 30))
            | Background c -> (codify (color_to_int c + 40))
                ^ (to_string_helper fg c s) ^ (codify (color_to_int bg + 40)))
      | Mudstring_meta (d, s) -> (match d with
          | Meta_room_title -> to_string_helper fg bg (Mudstring_mode
                (Foreground Green, Mudstring_list (Separator_none,
                    [Mudstring "["; s; Mudstring "]"])))
          | Meta_room_contents -> "Also here: " ^ (to_string_helper fg bg s)
          | Meta_room_exits -> "Exits: " ^ (to_string_helper fg bg s)
          | Meta_prompt -> "> " ^ (to_string_helper fg bg s)
          | _ -> to_string_helper fg bg s)
      | Mudstring_list (_, []) -> ""
      | Mudstring_list (_, [s]) -> to_string_helper fg bg s
      | Mudstring_list (how, s::l) ->
        if s = Mudstring_none then
          to_string_helper fg bg (Mudstring_list (how, l))
        else
          let sep = match how with
            | Separator_none -> ""
            | Separator_newline -> "\r\n"
            | Separator_space -> " "
            | Separator_comma -> if List.length l = 1 then ", and " else ", " in
          (to_string_helper fg bg s) ^ sep ^ (to_string_helper fg bg (Mudstring_list (how, l)))
      | Mudstring_condition (c, s1, s2) -> if looker == c
          then to_string_helper fg bg s1
          else to_string_helper fg bg s2
      | Mudstring_name o -> to_string_helper fg bg (o#short_description looker)
    in to_string_helper Normal Normal str
