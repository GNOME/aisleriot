/*
   Copyright © 2007, 2008 Christian Persch

   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published 
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <config.h>

#include <glib/gi18n.h>

#include "games-card.h"

static const char extra_cards[][12] = {
  "black_joker",
  "red_joker",
  "back",
  "slot"
};

static const char suites[][8] = {
  "club",
  "diamond",
  "heart",
  "spade"
};

static const char ranks[][6] = {
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10",
  "jack",
  "queen",
  "king"
};

/**
 * games_card_get_name_by_id_snprintf:
 * @buffer: the output buffer
 * @bufsize: the size of the output buffer
 * @card_id: the ID of the card
 *
 * Prints the identifier for the card @card into @buffer.
 *
 * Returns: the number of bytes which would be produced if the buffer
 * was large enough.
 */
int
games_card_get_name_by_id_snprintf (char *buffer,
                                    gsize bufsize,
                                    int card_id)
{
  int suit, rank, len;

  suit = card_id / 13;
  rank = card_id % 13;

  if (G_LIKELY (suit < 4)) {
    len = g_snprintf (buffer, bufsize, "%s-%s", suites[suit], ranks[rank]);
  } else {
    len = g_snprintf (buffer, bufsize, "%s", extra_cards[rank]);
  }

  return len;
}

/**
 * games_card_get_node_by_suit_and_rank_snprintf:
 * @buffer: the output buffer
 * @bufsize: the size of the output buffer
 * @card_id: the ID of the card
 *
 * Prints the identifier for the card @card into @buffer.
 *
 * Returns: the number of bytes which would be produced if the buffer
 * was large enough.
 */
int
games_card_get_node_by_suit_and_rank_snprintf (char *buffer,
                                               gsize bufsize,
                                               int suit,
                                               int rank)
{
  int len;

  if (G_LIKELY (suit < 4)) {
    len = g_snprintf (buffer, bufsize, "#%s_%s", ranks[rank], suites[suit]);
  } else {
    len = g_snprintf (buffer, bufsize, "#%s", extra_cards[rank]);
  }

  return len;
}

/**
 * games_card_get_name_by_id:
 * @card_id:
 *
 * Returns the name of the card @cardid
 *
 * Returns: a newly allocated string containing the identifier for @card_id
 */
char *
games_card_get_name_by_id (gint card_id)
{
  char name[128];

  games_card_get_name_by_id_snprintf (name, sizeof (name), card_id);

  return g_strdup (name);
}

/**
 * games_card_get_name:
 * @card:
 *
 * Returns: a localised name for @card, e.g. "Face-down card" or
 * "9 of clubs", etc.
 */
const char *
games_card_get_name (Card card)
{
  return NULL;
}
