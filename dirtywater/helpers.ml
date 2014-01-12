(*pp camlp4o *)
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

 helpers.ml : various non-game related functions that are needed by the game
   code for simple tasks *)

(* identity funciton *)
let identity (x : 'a) : 'a =
  x

(* turn a string into a list of chars *)
let rec explode (str : string) : char list =
  match str with
      "" -> []
    | s  -> (String.get s 0)::explode(String.sub s 1 ((String.length s) - 1))

(* turn a character into a string *)
let string_of_char (c : char) : string = String.make 1 c

(* turn a list of characters into a string *)
let rec implode (cl : char list) : string =
  match cl with
      []    -> ""
    | c::cs -> (string_of_char c) ^ implode cs

(* determines if sub matches the start of str *)
let start_of (sub : string) (str : string) : bool =
  try
    sub = (String.sub str 0 (String.length sub))
  with Invalid_argument _ -> false

(* start_of with order of arguments reversed *)
let starts_with str sub =
  start_of sub str

(* remove the trailing newline from a string if it exists *)
let chomp (s : string) : string =
  let len = String.length s in
  if (len > 0) && String.get s (len - 1) = '\n'
  then String.sub s 0 (len - 1)
  else s

(* trims given character from right side of string *)
let rec rtrim (str : string) (to_trim : char list) : string =
  let len = String.length str in
  if List.exists ((=) (String.get str (len - 1))) to_trim
  then rtrim (String.sub str 0 (len - 1)) to_trim
  else str

(* trims given characters from left side of string *)
let rec ltrim (str : string) (to_trim : char list) : string =
  if List.exists ((=) (String.get str 0)) to_trim
  then ltrim (String.sub str 1 (String.length str - 1)) to_trim
  else str

(* trims whitespace from each side of a string *)
let trim_ws (str : string) : string =
  let ws = [' ';'\n';'\r';'\t'] in
  ltrim (rtrim str ws) ws

(* like List.assoc, but takes a function instead of a value *)
let assoc_fun (func : 'a -> bool) (l : ('a * 'b) list) : 'b =
  let (_, value) = List.find (fun (key, _) -> func key) l in
  value

(* turn a list into a string of form: foo, bar, and baz *)
let rec add_commas (l : string list) : string =
  match l with
      str::[]      -> str
    | str1::[str2] -> str1 ^ ", and " ^ str2
    | str::rest    -> str ^ ", " ^ (add_commas rest)
    | []           -> ""

(* convert an incoming telnet string into an ASCII string *)
let telnet_to_ascii str =
  let rec cl_helper cl =
    let drop_first = function
        c::cs -> cl_helper cs
      | [] -> [] in
    match cl with
        [] -> []
      | '\255'::cmd::cs ->
          begin
            match cmd with
                '\251' -> drop_first cs
              | '\252' -> drop_first cs
              | '\253' -> drop_first cs
              | '\254' -> drop_first cs
              | '\255' -> '\255'::cl_helper cs
              | c      -> cl_helper cs
          end
      | c::cs -> c::(cl_helper cs) in
  implode (cl_helper (explode str))

(* converts an outgoing ASCII string to a telnet string *)
let ascii_to_telnet (str : string) : string =
  let reg = Str.regexp "\255" in
  Str.global_replace reg "\255\255" str

let rec map_some (f : 'a -> 'b option) (l : 'a list) : 'b list =
  match l with
  | x::xs -> begin
        match f x with
        | Some b -> b::(map_some f xs)
        | None -> map_some f xs
      end
  | [] -> []

let rec find_some (l : 'a option list) : 'a =
  match l with
  | [] -> raise Not_found
  | x::xs -> (match x with
        | None -> find_some xs
        | Some a -> a)

let rec map_to_stream (f : 'a -> 'b Stream.t) (l : 'a list) : 'b Stream.t =
  match l with
    | x::xs -> [< f x; map_to_stream f xs >]
    | []    -> [< >]

let rec stream_nth (n : int) (s : 'a Stream.t) : 'a option =
  try
    if n = 1 then Some (Stream.next s)
    else if n > 1 then (Stream.junk s; stream_nth (n - 1) s)
    else raise (Failure "sream_nth")
  with Stream.Failure -> None

let option_to_list (o : 'a option) : 'a list =
  match o with
  | Some x -> [x]
  | None -> []
