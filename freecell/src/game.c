/* game.c --
   Copyright (C) 1998 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
   USA */

/* Written by Changwoo Ryu <cwryu@adam.kaist.ac.kr>. */

#include <config.h>

#include <time.h>
#include <stdlib.h>
#include <assert.h>

#include <glib.h>
#include "card.h"
#include "game.h"

typedef enum _Part Part;
enum _Part
{
  PART_FREECELL, PART_DESTINATION, PART_FIELD
};
    

typedef struct _HistoryElement HistoryElement;
struct _HistoryElement
{
  Part from_part;
  int from_index;
  Part to_part;
  int to_index;

  /* When field->field, it has moved number of cards.  No meaning in the other
     cases.  */
  int more_info;
};

static GSList *history_push (GSList *history, Part from_part, int from_index, Part to_part, int to_index, int more_info);
static GSList *history_pop (GSList *history, Part *from_part, int *from_index, Part *to_part, int *to_index, int *more_info);


static int last_seed;

/* Allocate memory and initialize game data.  */
FREECELLGAME *
freecellgame_new (int freecells_number, int fields_number)
{ 
  return freecellgame_new_with_seed (freecells_number, fields_number, 
				     time (NULL));
}  

FREECELLGAME *
freecellgame_restart (int freecells_number, int fields_number)
{
  return freecellgame_new_with_seed (freecells_number, fields_number, 
				     last_seed);
}

FREECELLGAME *
freecellgame_new_with_seed (int freecells_number, int fields_number, int seed)
{
  int i;
  FREECELLGAME *new_game;
  DECK *tmp_deck;
  int destinations_number = 4;

  last_seed = seed;
  /* 1) Allocate memory.  */
  new_game = (FREECELLGAME *)g_malloc (sizeof(FREECELLGAME));
  
  new_game->freecells_number = freecells_number;
  new_game->freecells = (CARD **)g_malloc (freecells_number * sizeof(CARD *));
  for (i = 0; i < freecells_number; i++)
    new_game->freecells[i] = NULL;

  new_game->destinations_number = destinations_number;
  new_game->destinations = (DECK **)g_malloc (destinations_number * sizeof(DECK *));
  for (i = 0; i < destinations_number; i++)
    new_game->destinations[i] = deck_new(DECK_OPTION_NO_CARD);

  new_game->fields_number = fields_number;
  new_game->fields = (DECK **)g_malloc (fields_number * sizeof(DECK *));
  for (i = 0; i < fields_number; i++)
    new_game->fields[i] = deck_new (DECK_OPTION_NO_CARD);

  new_game->seed = seed;
  new_game->history = NULL;
  
  /* 2) Initialize game.  */
  tmp_deck = deck_new (DECK_OPTION_NO_JOKER);
  srand (seed);
  deck_shuffle(tmp_deck);

  i = 0;
  while (deck_number(tmp_deck) != 0)
    {
      deck_add_top (new_game->fields[i], deck_remove_top(tmp_deck));
      i = (i+1)%fields_number;
    }
  
  deck_delete (tmp_deck);
  
  return new_game;
}

void
freecellgame_delete_history(FREECELLGAME *freecellgame)
{
  GSList *list;

  list = freecellgame->history;
  while (list)
    {
      g_free (list->data);
      list = list->next;
    }

  g_slist_free (freecellgame->history);
  
  freecellgame->history = NULL;
}

/* Deallocate all allocated memory.  */
void
freecellgame_delete(FREECELLGAME *freecellgame)
{
  int i;

  for (i = 0; i < freecellgame->freecells_number; i++)
    if (freecellgame->freecells[i] != NULL)
      card_delete (freecellgame->freecells[i]);
  g_free (freecellgame->freecells);

  for (i = 0; i < freecellgame->destinations_number; i++)
    deck_delete (freecellgame->destinations[i]);
  g_free (freecellgame->destinations);

  for (i = 0; i < freecellgame->fields_number; i++)
    deck_delete (freecellgame->fields[i]);
  g_free (freecellgame->fields);

  freecellgame_delete_history(freecellgame);

  g_free(freecellgame);
}

CARD *
freecellgame_get_destination_top (FREECELLGAME *game, int index)
{
  DECK *deck;
  
  if (index > game->destinations_number)
    return NULL;

  deck = game->destinations[index];

  return deck_view_top (game->destinations[index]);
}

DECK *
freecellgame_get_field (FREECELLGAME *game, int index)
{
  if (index > game->fields_number)
    return NULL;

  return game->fields[index];
}

CARD *
freecellgame_get_freecell (FREECELLGAME *game, int index)
{
  if (index > game->freecells_number)
    return NULL;

  return game->freecells[index];
}


static int 
empty_freecells_number (FREECELLGAME *game)
{
  int count = 0;

  int i;

  for (i = 0; i < game->freecells_number; i++)
    {
      if (freecellgame_get_freecell (game, i) == NULL)
	count++;
    }
  return count;
}

static int
empty_fields_number (FREECELLGAME *game)
{
  int count = 0;

  int i;

  for (i = 0; i < game->fields_number; i++)
    {
      if (deck_number (freecellgame_get_field (game, i)) == 0)
        count++;
    }
  return count;
}

static int
can_be_on_in_field (CARD *a, CARD *b)
{
  if (!b)
    return 1;
  
  if (((card_is_red(a) && card_is_black(b))
       || (card_is_black(a) && card_is_red(b)))
      && (card_rank(a) == (card_rank(b) - 1)))
    return 1;
  
  return 0;
}

static int
can_be_on_in_destination (CARD *a, CARD *b)
{
  if (!b)
    {
      if (card_rank(a) == 1)
	return 1;
      else
	return 0;
    }
  
  if ((card_suit(a) == card_suit(b))
      && (card_rank(a) == (card_rank(b) + 1)))
    return 1;

  return 0;
}


int
freecellgame_can_move_field_to_destination (FREECELLGAME *game,
					    int from, int to)
{
  CARD *card;
  CARD *tmp_card;

  assert ((from >= 0) && (from < game->fields_number));
  assert ((to >= 0) && (to < game->destinations_number));

  card = deck_view_top(game->fields[from]);
  if (card == NULL)
    return 0;

  if (card_rank(card) == ACE)
    {
      if (deck_number (game->destinations[to]) == 0)
	return 1;
      else
	return 0;
    }

  tmp_card = deck_view_top (game->destinations[to]);
  if (can_be_on_in_destination(card, tmp_card))
    return 1;

  return 0;
}


int
freecellgame_can_move_field_to_freecell (FREECELLGAME *game,
					 int from, int to)
{
  CARD *card;

  assert ((from >= 0) && (from < game->fields_number));
  assert ((to >= 0) && (to < game->freecells_number));

  card = deck_view_top (game->fields[from]);
  if (card == NULL)
    return 0;

  if (game->freecells[to] != NULL)
    return 0;

  return 1;
}

int
freecellgame_can_move_field_to_field (FREECELLGAME *game,
				      int from, int to)
{
  CARD *card, *tmp_card;

  assert (from >= 0 && from < game->fields_number);
  assert (to >= 0 && to < game->fields_number);
  
  if (from == to)
    return 0;
  
  card = deck_view_top (game->fields[from]);
  if (card == NULL)
    return 0;

  if (deck_number (game->fields[to]) == 0)
    return 1;

  tmp_card = deck_view_top (game->fields[to]);

  if (can_be_on_in_field(card, tmp_card))
    return 1;

  return 0;
}

int
freecellgame_can_move_freecell_to_destination (FREECELLGAME *game,
					       int from, int to)
{
  CARD *card;
  CARD *tmp_card;

  assert (from >= 0 && from < game->freecells_number);
  assert (to >= 0 && to < game->destinations_number);
  
  card = game->freecells[from];
  if (card == NULL)
    return 0;

  if (card_rank(card) == ACE)
    {
      if (deck_number (game->destinations[to]) == 0)
	return 1;
      else 
	return 0;
    }

  tmp_card = deck_view_top (game->destinations[to]);
  if (can_be_on_in_destination(card, tmp_card))
    return 1;

  return 0;
}

int
freecellgame_can_move_freecell_to_freecell (FREECELLGAME *game,
					    int from, int to)
{
  assert (from >= 0 && from < game->freecells_number);
  assert (to >= 0 && to < game->freecells_number);
  
  if (from == to)
    return 0;

  if (game->freecells[from] == NULL)
    return 0;
  
  if (game->freecells[to] != NULL)
    return 0;
  
  return 1;
}

int
freecellgame_can_move_freecell_to_field (FREECELLGAME *game,
					 int from, int to)
{
  CARD *tmp_card;
  
  assert (from >= 0 && from < game->freecells_number);
  assert (to >= 0 && to < game->fields_number);
  
  if (game->freecells[from] == NULL)
    return 0;

  if (deck_number (game->fields[to]) == 0)
    return 1;

  tmp_card = deck_view_top (game->fields[to]);
  if (can_be_on_in_field(game->freecells[from], tmp_card))
    return 1;

  return 0;
}


/* Game movement control.  */


int
freecellgame_field_to_destination (FREECELLGAME *game,
				   int from, int to)
{
  CARD *card;
  
  assert (from >= 0 && from < game->fields_number);
  assert (to >= 0 && to < game->destinations_number);

  if (freecellgame_can_move_field_to_destination (game, from, to))
    {
      card = deck_view_top(game->fields[from]);
      deck_add_top (game->destinations[to], card);
      deck_remove_top (game->fields[from]);
      game->history = history_push (game->history, PART_FIELD, from,
				    PART_DESTINATION, to, 1);
      return 1;
    }
  else
    return -1;
}

int
freecellgame_field_to_freecell (FREECELLGAME *game,
				int from, int to)
{
  CARD *card;
  
  assert (from >= 0 && from < game->fields_number);
  assert (to >= 0 && to < game->fields_number);

  if (freecellgame_can_move_field_to_freecell (game, from, to))
    {
      card = deck_view_top(game->fields[from]);
      game->freecells[to] = card;
      deck_remove_top (game->fields[from]);
      game->history = history_push (game->history, PART_FIELD, from,
				    PART_FREECELL, to, 1);
      return 1;
    }
  else
    return -1;
}

int
freecellgame_field_to_field (FREECELLGAME *game,
			     int from, int to)
{
  CARD *card;
  
  assert (from >= 0 && from < game->fields_number);
  assert (to >= 0 && to < game->fields_number);

  if (freecellgame_can_move_field_to_field (game, from, to))
    {
      card = deck_view_top(game->fields[from]);
      deck_remove_top (game->fields[from]);
      deck_add_top (game->fields[to], card);
      game->history = history_push (game->history, PART_FIELD, from,
				    PART_FIELD, to, 1);
      return 1;
    }
  else
    return -1;
}

int
freecellgame_freecell_to_destination (FREECELLGAME *game,
				      int from, int to)
{
  assert (from >= 0 && from < game->freecells_number);
  assert (to >= 0 && to < game->destinations_number);
  
  if (freecellgame_can_move_freecell_to_destination(game, from, to))
    {
      deck_add_top (game->destinations[to], game->freecells[from]);
      game->freecells[from] = NULL;
      game->history = history_push (game->history, PART_FREECELL, from,
				    PART_DESTINATION, to, 1);
      return 1;
    }
  else
    return -1;
}

int
freecellgame_freecell_to_freecell (FREECELLGAME *game,
				   int from, int to)
{
  assert (from >= 0 && from < game->freecells_number);
  assert (to >= 0 && to < game->freecells_number);

  if (freecellgame_can_move_freecell_to_freecell(game, from, to))
    {
      game->freecells[to] = game->freecells[from];
      game->freecells[from] = NULL;
      game->history = history_push (game->history, PART_FREECELL, from,
				    PART_FREECELL, to, 1);
      return 1;
    }
  else
    return -1;
}

int
freecellgame_freecell_to_field (FREECELLGAME *game,
				int from, int to)
{
  assert (from >= 0 && from < game->freecells_number);
  assert (to >= 0 && to < game->fields_number);

  if (freecellgame_can_move_freecell_to_field (game, from, to))
    {
      deck_add_top (game->fields[to], game->freecells[from]);
      game->freecells[from] = NULL;
      game->history = history_push (game->history, PART_FREECELL, from,
				    PART_FIELD, to, 1);
      return 1;
    }
  else
    return -1;
}


int
freecellgame_field_to_empty_freecell (FREECELLGAME *game,
				      int from,
				      int *empty_index)
{
  int i;

  assert (from >= 0 && from < game->fields_number);

  *empty_index = -1;

  for (i = 0; i < game->freecells_number; i++)
    if (freecellgame_can_move_field_to_freecell (game, from, i))
      {
	*empty_index = i;
	return freecellgame_field_to_freecell (game, from, i);
      }

  return -1;
}

static int
max_moveable_cards (int cells,
		    int fields)
{
  int blocks;
  int size; 
  blocks=(1+(fields*(fields+1))/2);
  size=(cells+1);
  return blocks*size;
}

int
freecellgame_can_move_field_to_field_sequence (FREECELLGAME *game,
					       int from,
					       int to)
{
  int count;
  int empty_fcs;
  int empty_flds;
  DECK *from_deck, *to_deck;
  CARD *to_card;

  int i;

  assert (from >= 0 && from < game->fields_number);
  assert (to >= 0 && to < game->fields_number);

  empty_fcs = empty_freecells_number (game);
  empty_flds = empty_fields_number (game);

  from_deck = game->fields[from];
  if (deck_number(from_deck) == 0)
    return 0;

  to_deck = game->fields[to];
  if (deck_number(to_deck) == 0)
    {
      count = 1;
      for (i = 0; i < deck_number(from_deck); i++)
	{
	  if (count == max_moveable_cards (empty_fcs, empty_flds-1))
	    return count;

	  if ((i == (deck_number(from_deck) - 1))
	      || (! can_be_on_in_field (deck_view(from_deck, i),
					deck_view(from_deck, i+1))))
	    return count;

	  count ++;
	}
      return 0;
    }
  else
    {
      to_card = deck_view_top (to_deck);
      count = 1;
      for (i = 0; i < deck_number(from_deck); i++)
	{
	  if (can_be_on_in_field (deck_view(from_deck, i), to_card))
	    {
	      if (count > max_moveable_cards (empty_fcs, empty_flds))
		return 0;
	      else
		return count;
	    }

	  if ((i == (deck_number(from_deck) - 1))
	      || (! can_be_on_in_field (deck_view(from_deck, i),
					deck_view(from_deck, i+1))))
	    return 0;
	  
	  count++;
	}
      return 0;
    }
}


int
freecellgame_field_to_field_sequence (FREECELLGAME *game,
				      int from,
				      int to)
{
  int count;
  int i, p;
  DECK *to_deck, *from_deck;
  
  assert (from >= 0 && from < game->fields_number);
  assert (to >= 0 && to < game->fields_number);

  count = freecellgame_can_move_field_to_field_sequence(game, from, to);
  if (count)
    {
      from_deck = game->fields[from];
      to_deck = game->fields[to];

      p = deck_number(game->fields[from]) - count;
      for (i = 0; i < count; i++)
	deck_add_top (to_deck, deck_remove(from_deck, p));
      game->history = history_push (game->history, PART_FIELD, from,
				    PART_FIELD, to, count);
      return 1;
    }
  else
    return -1;
}


static int
can_to_destination_auto (FREECELLGAME *game,
			 CARD *card, int *dest_index)
{
  int i;
  int count = 0;
  
  CARD *c;

  *dest_index = -1;


  if (card_rank(card) <= 2)
    {
      for (i = 0; i < 4; i++)
	{
	  c = freecellgame_get_destination_top(game, i);
	  if (can_be_on_in_destination(card, c))
	    {
	      *dest_index = i;
	      return 1;
	    }
	}
    }
  else
    {
      for (i = 0; i < 4; i ++)
	{
	  c = freecellgame_get_destination_top(game, i);

	  if (c)
	    {
	      if (card_suit(c) == card_suit(card))
		{
		  if (can_be_on_in_destination(card, c))
		    *dest_index = i;
		  else
		    return 0;
		}
	      else
		{
		  if ((card_is_red(card) && card_is_black(c))
		      || (card_is_black(card) && card_is_red(c)))
		    {
		      count++;
		      if (card_rank(c) < (card_rank(card) - 1))
			return 0;
		    }
		}
	    }
	}
      if (count < 2)
	return 0;
      if (*dest_index < 0)
	return 0;
      
      return 1;
    }
  return 0;
}


int
freecellgame_to_destination_auto (FREECELLGAME *game,
				  int *is_from_freecell,
				  int *from_index, int *to_index)
{
  int i;

  int dest_index;
  DECK *deck;
  CARD *card;

  for (i = 0; i < 4; i++)
    {
      card = freecellgame_get_freecell(game, i);
      if (card
	  && can_to_destination_auto (game, card, &dest_index))
	{
	  *is_from_freecell = 1;
	  *from_index = i;
	  *to_index = dest_index;
	  freecellgame_freecell_to_destination(game, i, dest_index);
	  return 1;
	}
    }

  for (i = 0; i < 8; i++)
    {
      deck = freecellgame_get_field(game, i);
      card = deck_view_top(deck);
      if (card
	  && can_to_destination_auto (game, card, &dest_index))
	{
	  *is_from_freecell = 0;
	  *from_index = i;
	  *to_index = dest_index;
	  freecellgame_field_to_destination(game, i, dest_index);
	  return 1;
	}
    }
  return -1;
}

int
freecellgame_is_finished (FREECELLGAME *game)
{
  int i;
  
  for (i = 0; i < game->freecells_number; i++)
    if (game->freecells[i] != NULL)
      return 0;
  for (i = 0; i < game->fields_number; i++)
    if (deck_number(game->fields[i]) != 0)
      return 0;
  return 1;
}


int
freecellgame_is_there_no_way (FREECELLGAME *game)
{
  int i, j;

  if (empty_freecells_number (game) > 0)
    return 0;

  for (i = 0; i < game->freecells_number; i++)
    {
      for (j = 0; j < game->destinations_number; j++)
	if (freecellgame_can_move_freecell_to_destination(game, i, j))
	  return 0;
    }
  
  for (i = 0; i < game->fields_number; i++)
    {
      for (j = 0; j < game->destinations_number; j++)
	if (freecellgame_can_move_field_to_destination(game, i, j))
	  return 0;
    }
  
  for (i = 0; i < game->freecells_number; i++)
    {
      for (j = 0; j < game->fields_number; j++)
	if (freecellgame_can_move_freecell_to_field(game, i, j))
	  return 0;
    }

  for (i = 0; i < game->fields_number; i++)
    {
      for (j = 0; j < game->fields_number; j++)
	if (freecellgame_can_move_field_to_field(game, i, j))
	  return 0;
    }

  return 1;
}


int
freecellgame_undo (FREECELLGAME *game)
{
  Part from_part, to_part;
  int from_index, to_index, more_info;
  CARD *card = NULL;
  int i, tmp;
  
  if (! game->history)
    return -1;

  game->history = history_pop (game->history, &from_part, &from_index,
			       &to_part, &to_index, &more_info);

  if ((from_part == PART_FIELD) && (to_part == PART_FIELD))
    {
      tmp = deck_number (game->fields[to_index]) - more_info;
      for (i = (more_info - 1); i >= 0; i--)
	deck_add_top (game->fields[from_index],
		      deck_remove (game->fields[to_index], tmp));

      return more_info;
    }
  else
    {
      switch (to_part)
	{
	case PART_FREECELL:
	  card = game->freecells[to_index];
	  game->freecells[to_index] = NULL;
	  break;
	case PART_DESTINATION:
	  card = deck_view_top (game->destinations[to_index]);
	  deck_remove_top (game->destinations[to_index]);
	  break;
	case PART_FIELD:
	  card = deck_view_top (game->fields[to_index]);
	  deck_remove_top (game->fields[to_index]);
	  break;
	default:
	  g_error("Unknown to_part for this card");
	}
      
      switch (from_part)
	{
	case PART_FREECELL:
	  game->freecells[from_index] = card;
	  break;
	case PART_DESTINATION:
	  deck_add_top (game->destinations[from_index], card);
	  break;
	case PART_FIELD:
	  deck_add_top (game->fields[from_index], card);
	  break;
	}
      return 1;
    }
}


static GSList *
history_push (GSList *history, Part from_part, int from_index,
	      Part to_part, int to_index, int more_info)
{
  HistoryElement *he;

  he = (HistoryElement *) g_malloc (sizeof (HistoryElement));
  he->from_part = from_part;
  he->from_index = from_index;
  he->to_part = to_part;
  he->to_index = to_index;
  he->more_info = more_info;
  
  return g_slist_prepend (history, he);
}


static GSList *
history_pop (GSList *history, Part *from_part, int *from_index,
	     Part *to_part, int *to_index, int *more_info)
{
  HistoryElement *he;
  GSList *list;

  he = history->data;
  if (from_part)
    *from_part = he->from_part;
  if (from_index)
    *from_index = he->from_index;
  if (to_part)
    *to_part = he->to_part;
  if (to_index)
    *to_index = he->to_index;
  if (more_info)
    *more_info = he->more_info;

  g_free (he);
  list = history->next;
  g_slist_free_1 (history);
  return list;
}






#ifdef DEBUG
#include <stdio.h>

extern void card_dump (CARD *card);
extern void deck_dump (DECK *deck);

void
freecellgame_dump (FREECELLGAME *game)
{
  int i;
  
  printf ("* Freecells\n");
  for (i = 0; i < game->freecells_number; i++)
    {
      printf ("** Freecell %d\n", i);
      card_dump(game->freecells[i]);
    }
  printf ("* Destinations\n");
  for (i = 0; i < game->destinations_number; i++)
    {
      printf ("** Destination %d\n", i);
      deck_dump(game->destinations[i]);
    }
  printf ("* Fields\n");
  for (i = 0; i < game->fields_number; i++)
    {
      printf ("** Field %d\n", i);
      deck_dump(game->fields[i]);
    }
}
#endif /* DEBUG */









