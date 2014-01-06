open Race
open Types

let () =
  ignore (new race "normalhuman" "human"
    (new bodypartDef Head [
      new bodypartDef Torso [
        new bodypartDef (Arm Left) [new bodypartDef (Hand Left) []];
        new bodypartDef (Arm Right) [new bodypartDef (Hand Right) []];
        new bodypartDef (Leg Left) [new bodypartDef (Foot Left) []];
        new bodypartDef (Leg Right) [new bodypartDef (Foot Right) []]]]))
