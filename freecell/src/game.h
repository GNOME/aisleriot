/* game.h -- Freecell game.
   Copyright (C) 1997 Changwoo Ryu

   This program is free software; you can redistribute it and'or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Changwoo Ryu <cwryu@eve.kaist.ac.kr>. */

#ifndef __FREECELL__GAME_H
#define __FREECELL__GAME_H

#include "card.h"

typedef struct __FREECELLGAME
{
  int freecells_number;
  CARD **freecells;

  int destinations_number;	/* It can be changed ?? */
  DECK **destinations;
  
  int fields_number;
  DECK **fields;

  int seed;
} FREECELLGAME;


/* constructor/destructor.  */
extern FREECELLGAME *freecellgame_new (int freecells_number, int fields_number);
extern FREECELLGAME *freecellgame_new_with_seed (int freecells_number, int fields_number, int seed);
extern void freecellgame_delete (FREECELLGAME *game);

/* for getting game's current information */
extern CARD *freecellgame_get_destination_top (FREECELLGAME *game, int index);
extern DECK *freecellgame_get_field (FREECELLGAME *game, int index);
extern CARD *freecellgame_get_freecell (FREECELLGAME *game, int index);

extern int freecellgame_can_move_field_to_destination (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_field_to_freecell (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_field_to_field (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_freecell_to_destination (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_freecell_to_freecell (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_freecell_to_field (FREECELLGAME *game, int from, int to);


/* simple movement.  */
extern int freecellgame_field_to_destination (FREECELLGAME *game, int from, int to);
extern int freecellgame_field_to_freecell (FREECELLGAME *game, int from, int to);
extern int freecellgame_field_to_field (FREECELLGAME *game, int from, int to);
extern int freecellgame_freecell_to_freecell (FREECELLGAME *game, int from, int to);
extern int freecellgame_freecell_to_field (FREECELLGAME *game, int from, int to);
extern int freecellgame_freecell_to_destination (FREECELLGAME *game, int from, int to);

/* complex movement. */
extern int freecellgame_field_to_empty_freecell (FREECELLGAME *game, int field_index, int *empty_index);
extern int freecellgame_field_to_field_sequence (FREECELLGAME *game, int from_field_index, int to_field_index);
extern int freecellgame_to_destination_auto (FREECELLGAME *game, int *is_from_freecell, int *from_index, int *to_index);

/* predicates.  */
extern int freecellgame_can_move_field_to_destination (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_field_to_freecell (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_field_to_field (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_freecell_to_destination (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_freecell_to_freecell (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_freecell_to_field (FREECELLGAME *game, int from, int to);
extern int freecellgame_can_move_field_to_field_sequence (FREECELLGAME *game, int from, int to);
extern int freecellgame_is_finished (FREECELLGAME *game);
extern int freecellgame_is_there_no_way (FREECELLGAME *game);


#endif /* __FREECELL__GAME_H */
