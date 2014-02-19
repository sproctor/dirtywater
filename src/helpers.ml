(*
 Copyright 2014 Sean Proctor, Mike MacHenry

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

(* turn a string into a list of chars *)
let rec explode (str : string) : char list =
  match str with
  | "" -> []
  | s  -> (String.get s 0)::explode(String.sub s 1 ((String.length s) - 1))

(* turn a character into a string *)
let string_of_char (c : char) : string = String.make 1 c

(* turn a list of characters into a string *)
let rec implode (cl : char list) : string =
  match cl with
  | []    -> ""
  | c::cs -> (string_of_char c) ^ implode cs

(* determines if sub matches the start of str *)
let start_of (sub : string) (str : string) : bool =
  try
    sub = (String.sub str 0 (String.length sub))
  with Invalid_argument _ -> false

(* start_of with order of arguments reversed *)
let starts_with str sub =
  start_of sub str

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

let option_to_list (o : 'a option) : 'a list =
  match o with
  | Some x -> [x]
  | None -> []

(* Borrowed from http://rosettacode.org/wiki/Read_entire_file#OCaml *)
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let rec print_node (depth: int) (node: YamlNode.t) =
  for i = 1 to depth do print_string "    " done;
  match node with
  | YamlNode.SCALAR (uri, value) -> print_endline ("SCALAR (" ^ uri ^ ") " ^ value)
  | YamlNode.SEQUENCE (uri, nodes) ->
      print_endline ("SEQUENCE (" ^ uri ^ ")");
      List.iter (print_node (depth + 1)) nodes
  | YamlNode.MAPPING (uri, map) ->
      print_endline ("MAPPING (" ^ uri ^ ")");
      let print_mapping (key, value) =
        for i = 1 to depth + 1 do print_string "    " done;
        print_endline "MAPPING A:";
        print_node (depth + 1) key;
        for i = 1 to depth + 1 do print_string "    " done;
        print_endline "MAPPING B:";
        print_node (depth + 1) value in
      List.iter print_mapping map

let ignore_int (_ : int) : unit = ()
