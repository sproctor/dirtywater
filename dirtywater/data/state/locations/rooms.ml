open Location
open State
open Types

let () =
  let loc1001 = new simple_location 1001 "Starting Room" "...with a simple description"
      [(new direction_portal North 1002)] in
    let rock = templates#create_tangible "rock" (loc1001 :> container) in
    loc1001#add rock On;
  ignore (new simple_location 1002 "Another Room" "This room is in all ways inferior to the starting room. You should leave now."
     [new direction_portal South 1001;
      new direction_portal NorthEast 1003]);
  ignore (new simple_location 1003 "A dark room" "This room is dark and mysterious. You can't tell what has gone on here." [new direction_portal SouthWest 1002])
