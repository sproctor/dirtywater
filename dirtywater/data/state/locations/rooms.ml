open Types
open Convenience
open Template_collection

let () =
  let loc1001 = create_room 1001 "Starting Room" "...with a simple description"
      [North, 1002] in
  let rock = templates#create_tangible "rock" (loc1001 :> container) in
  loc1001#add rock On;
  ignore (create_room 1002 "Another Room"
     "This room is in all ways inferior to the starting room. You should leave now."
     [South, 1001; NorthEast, 1003]);
  ignore (create_room 1003 "A dark room"
    "This room is dark and mysterious. You can't tell what has gone on here."
    [SouthWest, 1002])
