/*
   Copyright © 2004 Callum McKenzie
   Copyright © 2007 Christian Persch

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

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

/* Common definitions for all card handling routines. */

#ifndef GAMES_CARD_COMMON
#define GAMES_CARD_COMMON

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
} Card;

typedef int _games_card_size_assert[sizeof (Card) == sizeof (guint8) ? 1 : -1]; /* static assertion */

#define CARD(c)               ((Card) c)
#define CARD_UINT(c)          (c.value)

#define CARD_GET_SUIT(c)      (c.attr.suit)
#define CARD_GET_RANK(c)      (c.attr.rank)
#define CARD_GET_FACE_DOWN(c) (c.attr.face_down)
#define CARD_FACE_UP(c)       (!CARD_FACE_DOWN(c))

#define POINTER_TO_CARD(ptr)  ((Card) (guint8) GPOINTER_TO_UINT (ptr))
#define CARD_TO_POINTER(card) (GUINT_TO_POINTER((guint) card.value))

/* Some defines */

#define SCALABLE_CARDS_DIR    COMMON_DATADIR G_DIR_SEPARATOR_S "cards"
#define PRERENDERED_CARDS_DIR COMMON_DATADIR G_DIR_SEPARATOR_S "card-themes"

#ifdef HAVE_MAEMO
#define GAMES_CARD_THEME_DEFAULT "paris"
#else
#define GAMES_CARD_THEME_DEFAULT "bonded"
#endif

#endif /* GAMES_CARD_COMMON */
