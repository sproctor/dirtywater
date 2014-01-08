open Location
open State
open Types

let () =
  let loc1001 = new location 1001 "Starting Room" "...with a simple description"
      [(create_portal North 1002)] in
    let rock = tangibles#get 1001 in
    loc1001#add rock;
    rock#set_parent (Some (loc1001 :> iContainer));
  ignore (new location 1002 "Another Room" "This room is in all ways inferior to the starting room. You should leave now."
     [create_portal South 1001;
      create_portal NorthEast 1003]);
  ignore (new location 1003 "A dark room" "This room is dark and mysterious. You can't tell what has gone on here." [create_portal SouthWest 1002])
