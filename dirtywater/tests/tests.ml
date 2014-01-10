open Helpers
open Types
open Lexer
open Parser
open Base
open Container
open Tangible
open Creature
open Location
open Character

let failure_count = ref 0

let test_count = ref 0

let test (str : string) (f : 'a -> string) (a : 'a) (b : 'a) : unit =
  test_count := !test_count + 1;
  if a <> b
  then begin
    print_string ("FAILED:\n" ^ str ^ "\nresult: " ^ (f a) ^ "\nexpected: "
                    ^ (f b));
    print_newline();
    failure_count := !failure_count + 1
  end
  else begin
    print_string ("Passed: " ^ str);
    print_newline()
  end

let string_of_token a : string =
  match a with
      WITH -> "WITH"
    | WORD (x) -> "WORD (" ^ x ^ ")"
    | _ -> ""

let print_list (f : 'a -> string) (a : 'a list) =
  let strings = List.map f a in
  String.concat "; " strings

let make_tokens str =
  let buf = Lexing.from_string str in
  let rec aux() =
    let t = (default_lexer buf) in
    if t = EOF then []
    else t::(aux ())
  in aux()

let iden x = x

let print_object_option (obj : object_desc option) : string =
  match obj with
      Some o -> object_desc_to_string o
    | None   -> "Default"

let print_command (cmd : player_command) : string =
  match cmd with
      Player_attack (x, y) -> "attack (" ^ print_object_option x ^ ", "
                       ^ print_object_option y ^ ")"
    | Player_take x -> "take (" ^ object_desc_to_string x ^ ")"
    | _ -> ""

let print_tangible (thing : iTangible) : string =
  "tangible: " ^ thing#get_name

let _ =
  print_string "Begining Unit Tests"; print_newline();
  (*test "testing lexer: \"quit\""
       (print_list string_of_token)
       (make_tokens "quit")
       [QUIT];
  test "testing lexer: \"  quit\""
       (print_list string_of_token)
       (make_tokens "  quit")
       [QUIT];
  test "testing lexer: \"quit\\r\\n\""
       (print_list string_of_token)
       (make_tokens "quit\r\n")
       [QUIT];
  test "testing lexer: \"quit bar baz\""
       (print_list string_of_token)
       (make_tokens "quit bar baz")
       [QUIT; WORD "bar"; WORD "baz"];*)
  test "testing starts_with on an empty string" string_of_bool
       (starts_with "" "g") false;
  test "testing starts_with on an empty string" string_of_bool
       (starts_with "foo" "") true;
  test "testing starts_with" string_of_bool
       (starts_with "foo" "f") true;
  test "testing starts_with" string_of_bool
       (starts_with "foo" "fo") true;
  test "testing starts_with" string_of_bool
       (starts_with "foo" "foo") true;
  test "testing starts_with with string that is too long" string_of_bool
       (starts_with "attack" "attackl") false;
  (* The following test cases describe the intended behavior of the parser
     as a whole. *)
  test "testing parser: \"quit\\r\\n\""
       print_command
       (parse_command "quit\r\n")
       (Player_quit);
  test "testing parser: \"wait 5\\r\\n\""
       print_command
       (parse_command "wait 5\r\n")
       (Player_wait (Some 5));
  (*test "testing lexer: \"attack foo\\r\\n\""
       (print_list string_of_token)
       (make_tokens "attack foo\r\n")
       [ATTACK; WORD "foo"];*)
  test "testing parser: \"attack foo\\r\\n\""
       print_command
       (parse_command "attack foo\r\n")
       (Player_attack (Some (ObjectDescBase (None, [], "foo")), None));
  (*test "testing lexer: \"Attack baz bar foo with blah\\r\\n\""
       (print_list string_of_token)
       (make_tokens "attack baz bar foo with blah\r\n")
       [ATTACK; WORD "baz"; WORD "bar"; WORD "foo"; WITH; WORD "blah"];*)
  test "testing parser: \"attack baz bar foo with blah\\r\\n\""
       print_command
       (parse_command "attack baz bar foo with blah\r\n")
       (Player_attack (Some (ObjectDescBase (None, ["baz";"bar"], "foo")),
                    Some (ObjectDescBase (None, [], "blah"))));
  test "testing parser: \"attack the baz bar foo with a blah\\r\\n\""
       print_command
       (parse_command "attack the baz bar foo with a blah\r\n")
       (Player_attack (Some (ObjectDescBase (None, ["baz";"bar"], "foo")),
                    Some (ObjectDescBase (None, [], "blah"))));
                    
  (* FIXME: readd this when it's implemented
  test "testing relative location with no container"
       print_command
       (parse_command "take the blue book near the table")
       (Player_take (ObjectDescRelative ((ObjectDescBase (["blue"],"book")),
                                 (Near ([],"table")))));*)

  (* These test cases check to make sure that the context sensitive
     expansion of the cardinal directions are done in the right place *)
  test "testing n"
       print_command
       (parse_command "n")
       (Player_move (ExitDescDir North));
  test "testing north"
       print_command
       (parse_command "north")
       (Player_move (ExitDescDir North));
  test "testing ne"
       print_command
       (parse_command "ne")
       (Player_move (ExitDescDir NorthEast));
  test "testing go n"
       print_command
       (parse_command "go n")
       (Player_move (ExitDescObj (ObjectDescBase (None, [], "n"))));
  test "testing attack n"
       print_command
       (parse_command "attack n")
       (Player_attack (Some (ObjectDescBase (None, [], "n")), None));
  test "testing attack n with e"
       print_command
       (parse_command "attack n with e")
       (Player_attack ((Some (ObjectDescBase (None, [], "n"))),
                       (Some (ObjectDescBase (None, [], "e")))));
  test "testing civil war case"
       print_command
       (parse_command "attack south with north")
       (Player_attack ((Some (ObjectDescBase (None, [], "south"))),
                       (Some (ObjectDescBase (None, [], "north")))));
  let t = new tangible 100 [] "t" "t" "tangible: t" in
  (*test "testing containment add/get"
    (print_list print_tangible)
    (let con = new containment in con#add t; con#get)
    [t];
  test "testing containment remove"
    (print_list print_tangible)
    (let con = new containment in con#add t; con#remove t; con#get)
    [];
  let t2 = new tangible ([], "t2") "t2" "tangible: t2" [] in
  test "testing containment multiple add"
    (print_list print_tangible)
    (let con = new containment in con#add t; con#add t2; con#get)
    [t2; t];
  test "testing containment multiple remove"
    (print_list print_tangible)
    (let con = new containment in con#add t; con#add t2; con#remove t;
      con#remove t2; con#get)
    [];
  test "testing containment single remove, multiple items"
    (print_list print_tangible)
    (let con = new containment in con#add t; con#add t2; con#remove t; con#get)
    [t2];
  test "testing tangible add/get"
    (print_list print_tangible)
    (let tan = new tangible ([], "tan") "tan" "tangible: tan" [In; On] in
      tan#add In t; tan#get In)
    [t];
  test "testing tangible remove"
    (print_list print_tangible)
    (let tan = new tangible ([], "tan") "tan" "tangible: tan" [In; On] in
      tan#add In t; tan#remove t; tan#get In)
    [];
  test "testing tangible multiple add"
    (print_list print_tangible)
    (let tan = new tangible ([], "tan") "tan" "tangible: tan" [In; On] in
      tan#add In t; tan#add In t2; tan#get In)
    [t2; t];
  test "testing tangible multiple remove"
    (print_list print_tangible)
    (let tan = new tangible ([], "tan") "tan" "tangible: tan" [In; On] in
      tan#add In t; tan#add In t2; tan#remove t; tan#remove t2; tan#get In)
    [];
  test "testing tangible single remove, multiple items"
    (print_list print_tangible)
    (let tan = new tangible ([], "tan") "tan" "tangible: tan" [In; On] in
      tan#add In t; tan#add In t2; tan#remove t; tan#get In)
    [t2];
  let move_tan = new tangible ([], "move_tan") "move_tan" "tangible: move_tan"
    [In; On] in
  test "testing tangible move_to"
    (print_list print_tangible)
    (t2#move_to [((move_tan :> iContainer), On)]; move_tan#get On)
    [t2];
  let h = new hand Left in
  test "testing hand add/get"
    (print_list print_tangible)
    (h#add In t; h#get In)
    [t];
  test "testing hand remove"
    (print_list print_tangible)
    (h#remove t; h#get In)
    [];
  let r = new location "room" "some stuff" 0.0 0.0 in
  let c = make_character "sean" "blah" r in
  test "test creature take"
    (print_list print_tangible)
    (c#take t; let h::_ = (c#get_body)#get_hands in h#get In)
    [t];
  test "testing creature get_inventory"
    (print_list print_tangible)
    (let [(_, _, result)] = List.filter
        (fun (bt, _, _) -> bt = (Hand Left)) c#get_inventory in result)
    [t];*)

(*  test (trim "foo" []) "foo";
  test (trim "  foo\r\n" []) "  foo";
  test (trim "foo\r" []) "foo";
  test (trim "\n\rfoo" []) "\n\rfoo";
  test (chomp "foo\n") "foo";
  test (chomp "foo") "foo";
  test (chomp "blah\nblah\n") "blah\nblah";
  test (chomp "\n") "";
  test (chomp "") "";*)
  print_string "Ending Unit Tests\n";
  print_string ("Failed " ^ (string_of_int !failure_count) ^ "/"
        ^ (string_of_int !test_count) ^ "\n")
