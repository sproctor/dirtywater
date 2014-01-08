open Race
open Types
open Creature

class hand_def (s : side) =
  object (self)
    inherit bodypart_def (Hand s) [] as super

    method create : bodypart =
      let part = super#create in
      part#add_container In (new hand_container (part :> iMud_object));
      part

  end
      
let () =
  ignore (new race "normalhuman" "human"
    (new bodypart_def Head [
      new bodypart_def Torso [
        new bodypart_def (Arm Left) [new hand_def Left];
        new bodypart_def (Arm Right) [new hand_def Right];
        new bodypart_def (Leg Left) [new bodypart_def (Foot Left) []];
        new bodypart_def (Leg Right) [new bodypart_def (Foot Right) []]]]))
