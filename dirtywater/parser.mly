%{
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

(* parser.mly: this file takes lexed input from a player and produces a
   a data structure that is an interim command data structure. that data
   structure will later be turned into a true command data structure by the
   player class (we cannot create a true command data structure here because we
   do not deal with the state of the game. a command data structure needs
   real references to existing objects
*)

open Types
open Helpers
open Debug

%}

%token ARTICLE WITH AND EOF
%token <int> NUMBER
%token <string> WORD
%token <char> LETTER
%token <int> ORDINAL

/* prepositions */
%token UNDER BEHIND ON IN FROM

%token <Types.emote> EMOTE
%token <string> TARGET

%start attack north east west south northeast northwest southeast southwest down up go drop take inventory look quit wait say
%type <Types.player_command> attack north east west south northeast northwest southeast southwest down up go drop take inventory look quit wait say

%%

attack:
    EOF                          { Player_attack (None, None)           }
  | obj_phrase EOF               { Player_attack (Some $1, None)        }
  | obj_phrase WITH obj_phrase EOF       { Player_attack (Some $1, Some $3)     }
  | WITH obj_phrase EOF          { Player_attack (None, Some $2)        }
;
north:
    EOF           { Player_move (ExitDescDir North)      }
;
east:
    EOF            { Player_move (ExitDescDir East)       }
;
south:
    EOF           { Player_move (ExitDescDir South)      }
;
west:
    EOF            { Player_move (ExitDescDir West)       }
northeast:
    EOF       { Player_move (ExitDescDir NorthEast)  }
;
northwest:
    EOF       { Player_move (ExitDescDir NorthWest)  }
;
southeast:
    EOF       { Player_move (ExitDescDir SouthEast)  }
;
southwest:
    EOF       { Player_move (ExitDescDir SouthWest)  }
;
up:
    EOF              { Player_move (ExitDescDir Up)         }
;
down:
    EOF            { Player_move (ExitDescDir Down)       }
;
go:
    obj_phrase EOF   { dlog 0 "got go result"; Player_move (ExitDescObj $1) }
  | EOF              { raise (Bad_command "Go where?") }
;
inventory:
    EOF { Player_inventory }
;
say:
    say_attributes WORD EOF { let (es, ts) = $1 in Player_say (es, ts, $2) }
;
take:
    obj_phrase EOF { Player_take $1 }
;
drop:
    obj_phrase EOF { Player_drop $1                       }
;
look:
    EOF            { Player_look None                     }
  | obj_phrase EOF { Player_look (Some (None, $1))        }
  | preposition obj_phrase EOF     { Player_look (Some (Some $1, $2))     }
;
quit:
    EOF            { Player_quit                          }
;
wait:
    EOF            { Player_wait None                     }
  | NUMBER EOF     { Player_wait (Some $1)                }
;
obj_phrase:
    obj_phrase preposition noun_phrase	{ ObjectDesc ($1, $2, $3) }
  | noun_phrase				{ ObjectDescBase $1 }
;
noun_phrase:
    article ord adjs WORD               { ($2, $3, $4) }
;
article:
    /* empty */                         { }
  | ARTICLE                             { }
;
ord:
    /* empty */                         { None }
  | ORDINAL                             { Some $1 }
;
adjs:
| /* empty */		{ [] }
| adjs WORD		{ $1@[$2] }
;
preposition:
| ON			{ Prep_on }
| IN			{ Prep_in }
| FROM			{ Prep_from }
| UNDER			{ Prep_under }
| BEHIND		{ Prep_behind }
;
say_attributes:
    /* empty */                 { ([], []) }
  | EMOTE say_attributes        { let (es, ts) = $2 in ($1::es, ts) }
  | TARGET say_attributes       { let (es, ts) = $2 in (es, $1::ts) }
;
