/*
  Copyright © 2007, 2008 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <glib/gi18n.h>

#include "ar-card.h"
#include "ar-card-private.h"

static const char extra_cards[] =
  "joker_black\0"
  "joker_red\0"
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
 * ar_card_get_node_by_suit_and_rank_snprintf:
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
ar_card_get_node_by_suit_and_rank_snprintf (char *buffer,
                                            gsize bufsize,
                                            int suit,
                                            int rank)
{
  int len;

  if (G_LIKELY (suit < 4)) {
    len = g_snprintf (buffer, bufsize, "#%s_%s",
                      suites + suite_offsets[suit],
                      ranks + rank_offsets[rank]);
  } else {
    len = g_snprintf (buffer, bufsize, "#%s",
                      extra_cards + extra_card_offsets[rank]);
  }

  return len;
}


/**
 * ar_card_get_legacy_node_by_suit_and_rank_snprintf:
 * @buffer: the output buffer
 * @bufsize: the size of the output buffer
 * @suit: the suit of the card
 * @rank: the rank of the card
 *
 * Prints the legacy identifier for the card @card into @buffer.
 *
 * Returns: the number of bytes which would be produced if the buffer
 * was large enough.
 */
int
ar_card_get_legacy_node_by_suit_and_rank_snprintf (char *buffer,
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
 * ar_card_get_node_by_id_snprintf:
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
ar_card_get_node_by_id_snprintf (char *buffer,
                                 gsize bufsize,
                                 int card_id)
{
  int suit, rank;

  suit = card_id / 13;
  rank = card_id % 13;

  return ar_card_get_node_by_suit_and_rank_snprintf (buffer, bufsize, suit, rank);
}

/**
 * ar_card_get_legacy_node_by_id_snprintf:
 * @buffer: the output buffer
 * @bufsize: the size of the output buffer
 * @card_id: the ID of the card
 *
 * Prints the legacy identifier for the card @card into @buffer.
 *
 * Returns: the number of bytes which would be produced if the buffer
 * was large enough.
 */
int
ar_card_get_legacy_node_by_id_snprintf (char *buffer,
                                        gsize bufsize,
                                        int card_id)
{
  int suit, rank;

  suit = card_id / 13;
  rank = card_id % 13;

  return ar_card_get_legacy_node_by_suit_and_rank_snprintf (buffer, bufsize, suit, rank);
}

/**
 * ar_card_get_name_by_id_snprintf:
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
ar_card_get_name_by_id_snprintf (char *buffer,
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
 * ar_card_get_name_by_id:
 * @card_id:
 *
 * Returns the name of the card @cardid
 *
 * Returns: a newly allocated string containing the identifier for @card_id
 */
char *
ar_card_get_name_by_id (gint card_id)
{
  char name[128];

  ar_card_get_name_by_id_snprintf (name, sizeof (name), card_id);

  return g_strdup (name);
}

/**
 * ar_card_get_localised_rank_symbol:
 * @rank: the card rank
 *
 * Returns: the localised rank card symbol
 */
const char *
ar_card_get_localised_rank_symbol (int rank)
{
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

  g_return_val_if_fail (rank >= AR_CARD_JOKER && rank <= AR_CARD_ACE_HIGH, NULL);

  return g_dpgettext2 (GETTEXT_PACKAGE, "card symbol", rank_texts[rank]);
}

guint
_ar_card_to_index (Card card)
{
  guint card_id;

  if (CARD_GET_FACE_DOWN (card)) {
    card_id = AR_CARD_BACK;
  } else if (G_UNLIKELY (CARD_GET_RANK (card) == 0)) {
    /* A joker */
    if (CARD_GET_SUIT (card) == AR_CARDS_CLUBS ||
        CARD_GET_SUIT (card) == AR_CARDS_SPADES) {
      /* A black joker. */
      card_id = AR_CARD_BLACK_JOKER;
    } else {
      /* A red joker. */
      card_id = AR_CARD_RED_JOKER;
    }
  } else {
    card_id = AR_CARD_ID (CARD_GET_SUIT (card), CARD_GET_RANK (card));
  }

  return card_id;
}

/* FIXMEchpe compactify & constify */
static const char *card_names[] = {
  N_("ace of clubs"),
  N_("two of clubs"),
  N_("three of clubs"),
  N_("four of clubs"),
  N_("five of clubs"),
  N_("six of clubs"),
  N_("seven of clubs"),
  N_("eight of clubs"),
  N_("nine of clubs"),
  N_("ten of clubs"),
  N_("jack of clubs"),
  N_("queen of clubs"),
  N_("king of clubs"),
  N_("ace of diamonds"),
  N_("two of diamonds"),
  N_("three of diamonds"),
  N_("four of diamonds"),
  N_("five of diamonds"),
  N_("six of diamonds"),
  N_("seven of diamonds"),
  N_("eight of diamonds"),
  N_("nine of diamonds"),
  N_("ten of diamonds"),
  N_("jack of diamonds"),
  N_("queen of diamonds"),
  N_("king of diamonds"),
  N_("ace of hearts"),
  N_("two of hearts"),
  N_("three of hearts"),
  N_("four of hearts"),
  N_("five of hearts"),
  N_("six of hearts"),
  N_("seven of hearts"),
  N_("eight of hearts"),
  N_("nine of hearts"),
  N_("ten of hearts"),
  N_("jack of hearts"),
  N_("queen of hearts"),
  N_("king of hearts"),
  N_("ace of spades"),
  N_("two of spades"),
  N_("three of spades"),
  N_("four of spades"),
  N_("five of spades"),
  N_("six of spades"),
  N_("seven of spades"),
  N_("eight of spades"),
  N_("nine of spades"),
  N_("ten of spades"),
  N_("jack of spades"),
  N_("queen of spades"),
  N_("king of spades")
};
#define AR_CARD_ID(suit, rank) ((13*(suit)) + ((rank-1)%13))

/**
 * ar_card_get_localized_name:
 * @card:
 *
 * Returns: a localised name for @card, e.g. "Face-down card" or
 * "9 of clubs", etc.
 */
const char *
ar_card_get_locale_name (Card card)
{
  guint rank, suit;

  if (CARD_GET_FACE_DOWN (card)) {
    return _("face-down card");
  };

  rank = CARD_GET_RANK (card);
  suit = CARD_GET_SUIT (card);

  if (G_UNLIKELY (rank == 0)) {
    /* A joker */
    if (suit == AR_CARDS_CLUBS ||
        suit == AR_CARDS_SPADES) {
      /* A black joker. */
      return _("black joker");
    } else {
      /* A red joker. */
      return _("red joker");
    }
  }

  return _(card_names[AR_CARD_ID(suit, rank)]);
}
