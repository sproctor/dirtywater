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

open Types
open Helpers

%}

%token ARTICLE WITH AND EOF
%token <int> NUMBER
%token <string> WORD
%token <int> ORDINAL
/* prepositions */
%token UNDER ON IN FROM OF BETWEEN NEAR
/* commands */
%token QUIT WAIT ATTACK GO LOOK TAKE INVENTORY DROP
%token NORTH SOUTH EAST WEST NORTHEAST NORTHWEST SOUTHEAST SOUTHWEST UP DOWN


%start main
%type <Types.player_command> main

%%

main:
    command EOF                  { $1 }
;
command:
  | ATTACK                              { Player_attack (None, None)           }
  | ATTACK obj_phrase			{ Player_attack (Some $2, None)        }
  | ATTACK obj_phrase WITH obj_phrase	{ Player_attack (Some $2, Some $4)     }
  | ATTACK WITH obj_phrase		{ Player_attack (None, Some $3)        }

  | NORTH				{ Player_move (ExitDescDir North)      }
  | SOUTH				{ Player_move (ExitDescDir South)      }
  | EAST				{ Player_move (ExitDescDir East)       }
  | WEST				{ Player_move (ExitDescDir West)       }
  | NORTHEAST				{ Player_move (ExitDescDir NorthEast)  }
  | NORTHWEST				{ Player_move (ExitDescDir NorthWest)  }
  | SOUTHEAST				{ Player_move (ExitDescDir SouthEast)  }
  | SOUTHWEST				{ Player_move (ExitDescDir SouthWest)  }
  | UP					{ Player_move (ExitDescDir Up)         }
  | DOWN				{ Player_move (ExitDescDir Down)       }

  | GO obj_phrase			{ Player_move (ExitDescObj $2)         }

  | INVENTORY                           { Player_inventory                     }

  | TAKE obj_phrase			{ Player_take $2                       }

  | DROP obj_phrase                     { Player_drop $2                       }

  | LOOK				{ Player_look None                     }
  | LOOK obj_phrase			{ Player_look (Some (None, $2))        }
  | LOOK preposition obj_phrase		{ Player_look (Some (Some $2, $3))     }

  | QUIT                                { Player_quit                          }

  | WAIT                                { Player_wait None                     }
  | WAIT NUMBER                         { Player_wait (Some $2)                }
;
obj_phrase:
    obj_phrase preposition noun_phrase	{ ObjectDesc ($1, $2, $3) }
  | obj_phrase location			{ ObjectDescRelative ($1, $2) }
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
    /* empty */		{ [] }
  | adjs WORD		{ $1@[$2] }
;
preposition:
    UNDER			{ Under }
  | ON				{ On }
  | IN				{ In }
  | FROM			{ From }
  | OF				{ Of }
;
location:
    BETWEEN noun_phrase AND noun_phrase	{ Between ($2, $4) }
  | NEAR noun_phrase			{ Near $2 }
;
