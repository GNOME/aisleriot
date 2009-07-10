/*
  Copyright © 2007, 2008 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <glib/gi18n.h>

#include "games-card.h"
#include "games-card-private.h"

static const char extra_cards[] =
  "black_joker\0"
  "red_joker\0"
  "back\0"
  "slot";
static const guint8 extra_card_offsets[] = {
  0, 12, 22, 27
};

static const char suites[] =
  "club\0"
  "diamond\0"
  "heart\0"
  "spade";
static const guint8 suite_offsets[] = {
  0, 5, 13, 19
};

static const char ranks[] =
  "1\0"
  "2\0"
  "3\0"
  "4\0"
  "5\0"
  "6\0"
  "7\0"
  "8\0"
  "9\0"
  "10\0"
  "jack\0"
  "queen\0"
  "king";
static const guint8 rank_offsets[] = {
  0, 2, 4, 6, 8, 10, 12, 14, 
  16, 18, 21, 26, 32
};

/**
 * games_card_get_node_by_suit_and_rank_snprintf:
 * @buffer: the output buffer
 * @bufsize: the size of the output buffer
 * @suit: the suit of the card
 * @rank: the rank of the card
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
    len = g_snprintf (buffer, bufsize, "#%s_%s",
                      ranks + rank_offsets[rank],
                      suites + suite_offsets[suit]);
  } else {
    len = g_snprintf (buffer, bufsize, "#%s",
                      extra_cards + extra_card_offsets[rank]);
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
games_card_get_node_by_id_snprintf (char *buffer,
                                    gsize bufsize,
                                    int card_id)
{
  int suit, rank;

  suit = card_id / 13;
  rank = card_id % 13;

  return games_card_get_node_by_suit_and_rank_snprintf (buffer, bufsize, suit, rank);
}

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
    len = g_snprintf (buffer, bufsize, "%s-%s",
                      suites + suite_offsets[suit],
                      ranks + rank_offsets[rank]);
  } else {
    len = g_snprintf (buffer, bufsize, "%s",
                      extra_cards + extra_card_offsets[rank]);
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
 * games_card_get_localised_rank_symbol:
 * @rank: the card rank
 *
 * Returns: the localised rank card symbol
 */
const char *
games_card_get_localised_rank_symbol (int rank)
{
#if GLIB_CHECK_VERSION (2, 18, 0)
  static const char *rank_texts[] = {
    /* Translators: this is the symbol that's on a Joker card */
    NC_("card symbol", "JOKER"),
    /* Translators: this is the symbol that's on an Ace card */
    NC_("card symbol", "A"),
    /* Translators: this is the symbol that's on a 2 card */
    NC_("card symbol", "2"),
    /* Translators: this is the symbol that's on a 3 card */
    NC_("card symbol", "3"),
    /* Translators: this is the symbol that's on a 4 card */
    NC_("card symbol", "4"),
    /* Translators: this is the symbol that's on a 5 card */
    NC_("card symbol", "5"),
    /* Translators: this is the symbol that's on a 6 card */
    NC_("card symbol", "6"),
    /* Translators: this is the symbol that's on a 7 card */
    NC_("card symbol", "7"),
    /* Translators: this is the symbol that's on a 8 card */
    NC_("card symbol", "8"),
    /* Translators: this is the symbol that's on a 9 card */
    NC_("card symbol", "9"),
    /* Translators: this is the symbol that's on a Jack card */
    NC_("card symbol", "J"),
    /* Translators: this is the symbol that's on a Queen card */
    NC_("card symbol", "Q"),
    /* Translators: this is the symbol that's on a King card */
    NC_("card symbol", "K"),
    /* Translators: this is the symbol that's on an Ace card */
    NC_("card symbol", "A"),
    /* Translators: this is the symbol that's on a 1 card */
    NC_("card symbol", "1")
  };

  g_return_val_if_fail (rank >= GAMES_CARD_JOKER && rank <= GAMES_CARD_ACE_HIGH, NULL);

  return g_dpgettext2 (GETTEXT_PACKAGE, "card symbol", rank_texts[rank]);

#else
  static const char rank_texts[][6] = { "JOKER", "A", "1", "2", "3", "4", "5", "6", "7", "8", "9", "J", "Q", "K", "A" };

  g_return_val_if_fail (rank >= GAMES_CARD_JOKER && rank <= GAMES_CARD_ACE_HIGH, NULL);

  return rank_texts[rank];
#endif /* GLIB >= 2.18.0 */
}

guint
_games_card_to_index (Card card)
{
  guint card_id;

  if (CARD_GET_FACE_DOWN (card)) {
    card_id = GAMES_CARD_BACK;
  } else if (G_UNLIKELY (CARD_GET_RANK (card) == 0)) {
    /* A joker */
    if (CARD_GET_SUIT (card) == GAMES_CARDS_CLUBS ||
        CARD_GET_SUIT (card) == GAMES_CARDS_SPADES) {
      /* A black joker. */
      card_id = GAMES_CARD_BLACK_JOKER;
    } else {
      /* A red joker. */
      card_id = GAMES_CARD_RED_JOKER;
    }
  } else {
    card_id = GAMES_CARD_ID (CARD_GET_SUIT (card), CARD_GET_RANK (card));
  }

  return card_id;
}
