/*
  Copyright © 2004 Callum McKenzie
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

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

/* Common definitions for all card handling routines. */

#ifndef AR_CARD_H
#define AR_CARD_H

#include <glib.h>

G_BEGIN_DECLS

/* __attribute__((__packed__)) is needed on some archs to make
 * the Card type below fit into one byte. See bug #512799.
 */
#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 6)
#define GNOME_GAMES_GNUC_PACKED \
  __attribute__((__packed__))
#else
#define GNOME_GAMES_GNUC_PACKED
#endif  /* !__GNUC__ */

/* A card */

/* Black Joker: value = 0, suit = spade or club
 * Red Joker: value = 0, suit = heart or diamond
 */
typedef union {
  guint8 value;
  struct {
    guint8 face_down : 1;
    guint8 suit : 2;
    guint8 rank : 4;
  } attr;
} GNOME_GAMES_GNUC_PACKED Card;

#ifndef __GI_SCANNER__
G_STATIC_ASSERT(sizeof (Card) == sizeof (guint8));
#endif

#define CARD(c)               ((Card) c)
#define CARD_UINT(c)          (c.value)

#define CARD_GET_SUIT(c)      (c.attr.suit)
#define CARD_GET_RANK(c)      (c.attr.rank)
#define CARD_GET_FACE_DOWN(c) (c.attr.face_down)

#define POINTER_TO_CARD(ptr)  ((Card) (guint8) GPOINTER_TO_UINT (ptr))
#define CARD_TO_POINTER(card) (GUINT_TO_POINTER((guint) card.value))

/* Some defines */

typedef enum {
  /* Cards */
  AR_CARD_JOKER = 0,
  AR_CARD_ACE = 1,
  AR_CARD_TWO = 2,
  AR_CARD_THREE = 3,
  AR_CARD_FOUR = 4,
  AR_CARD_FIVE = 5,
  AR_CARD_SIX = 6,
  AR_CARD_SEVEN = 7,
  AR_CARD_EIGHT = 8,
  AR_CARD_NINE = 9,
  AR_CARD_TEN = 10,
  AR_CARD_JACK = 11,
  AR_CARD_QUEEN = 12,
  AR_CARD_KING = 13,
  AR_CARD_ACE_HIGH = 14,

  /* Suites */
  AR_CARDS_CLUBS = 0,
  AR_CARDS_DIAMONDS = 1,
  AR_CARDS_HEARTS = 2,
  AR_CARDS_SPADES = 3,

  /* Jokers */
  AR_CARD_BLACK_JOKER = 52,
  AR_CARD_RED_JOKER = 53,

  /* Special */
  AR_CARD_BACK = 54,
  AR_CARD_SLOT = 55,
  AR_CARDS_TOTAL = 56,
} ArCardIDType;

int ar_card_get_node_by_suit_and_rank_snprintf (char *buffer,
                                                gsize bufsize,
                                                int suit,
                                                int rank);

int ar_card_get_legacy_node_by_suit_and_rank_snprintf (char *buffer,
                                                       gsize bufsize,
                                                       int suit,
                                                       int rank);

int ar_card_get_node_by_id_snprintf (char *buffer,
                                     gsize bufsize,
                                     int card_id);

int ar_card_get_legacy_node_by_id_snprintf (char *buffer,
                                            gsize bufsize,
                                            int card_id);

int ar_card_get_name_by_id_snprintf (char *buffer,
                                        gsize bufsize,
                                        int card_id);

char * ar_card_get_name_by_id (int card_id);

const char * ar_card_get_localised_rank_symbol (int rank);

const char * ar_card_get_locale_name (Card card);

G_END_DECLS

#endif /* !AR_CARD_H */
