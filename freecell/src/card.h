/* card.h --
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

#ifndef __FREECELL_CARD_H
#define __FREECELL_CARD_H
#include <gdk-card-image.h>

typedef GdkCardSuit CARD_SUIT;
typedef GdkCardRank CARD_RANK;
/* Don't modify this structure directly.  */
typedef struct __CARD
{
  CARD_SUIT suit;
  CARD_RANK rank;
} CARD;

/* CARD low level interface.  Direct access to DECK structure must be
   ONLY by these functions.  */

extern CARD *card_new (CARD_SUIT suit, CARD_RANK rank);
extern void card_delete (CARD *card);
extern CARD_SUIT card_suit (CARD *card);
extern CARD_RANK card_rank (CARD *card);

/* CARD high level interface.  */

extern int card_is_red (CARD *card);
extern int card_is_black (CARD *card);
extern int card_is_joker (CARD *card);
extern int card_is_equal (CARD *card1, CARD *card2);
     

/* Don't modify this structure directly.  */
typedef struct __DECK
{
  CARD **cards;
  int cards_number;
  int maximum_size;
} DECK;

#define DECK_OPTION_NORMAL 0
#define DECK_OPTION_NO_JOKER 1
#define DECK_OPTION_ONE_JOKER 2
#define DECK_OPTION_NO_CARD 3

/* DECK low level interface.  Direct access to DECK structure must be
   ONLY by these functions.  */

extern DECK *deck_new (unsigned int option);
extern void deck_delete (DECK *deck);
extern int deck_number (DECK *deck);
/* It may *crash* if "index" value is wrong.  */
extern CARD *deck_view (DECK *deck, int index);
extern CARD *deck_remove (DECK *deck, int index);
extern void deck_insert (DECK *deck, int index, CARD *card);

/* DECK high level interface.  */

extern void deck_add_top (DECK *deck, CARD *card);
extern void deck_add_bottom (DECK *deck, CARD *card);
extern CARD *deck_remove_top (DECK *deck);
extern CARD *deck_remove_bottom (DECK *deck);
extern CARD *deck_view_top (DECK *deck);
extern CARD *deck_view_bottom (DECK *deck);
extern void deck_shuffle (DECK *deck);





#endif /* __FREECELL_CARD_H */
