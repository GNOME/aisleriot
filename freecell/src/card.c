/* card.c --
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>
#include <glib.h>

#include "card.h"


/* Allocate a card and initialize it.  */
CARD *
card_new (CARD_SUIT suit, CARD_RANK rank)
{
  CARD *new_card;

  new_card = (CARD *)g_malloc (sizeof(CARD));
  new_card->suit = suit;
  new_card->rank = rank;
  return new_card;
}

/* Deallocate the card.  */
void
card_delete (CARD *card)
{
  g_free (card);
}

/* Return the shape of the card.  */
CARD_SUIT
card_suit (CARD *card)
{
  return card->suit;
}

/* Return the number of the card.  */
CARD_RANK
card_rank (CARD *card)
{
  return card->rank;
}

int
card_is_red (CARD *card)
{
  return ((card_suit(card) == HEART)
	  || (card_suit(card) == DIAMOND));
}

int
card_is_black (CARD *card)
{
  return ((card_suit(card) == SPADE)
	  || (card_suit(card) == CLUB));
}

int
card_is_joker (CARD *card)
{
  return (card_rank(card) == JOKER);
}

/* Examine two cards are the same card.  */
int
card_is_equal (CARD *card1, CARD *card2)
{
  return ((card_suit(card1) == card_suit(card2))
	  && (card_rank(card1) == card_rank(card2)));
}


#ifdef DEBUG

void
card_dump (CARD *card)
{
  if (card)
    printf ("{%d, %d}\n", card_suit(card), card_rank(card));
}
#endif /* DEBUG */

/* Make a new card deck.
   Allocate memory and initialize data.  */
DECK *
deck_new (unsigned int option)
{
  DECK *new_deck;
  int i, j;
  int k;

  new_deck = (DECK *) g_malloc (sizeof(DECK));
  switch (option)
    {
    case DECK_OPTION_NORMAL:
      new_deck->cards_number = 54;
      break;
    case DECK_OPTION_NO_JOKER:
      new_deck->cards_number = 52;
      break;
    case DECK_OPTION_ONE_JOKER:
      new_deck->cards_number = 53;
      break;
    case DECK_OPTION_NO_CARD:
      new_deck->cards_number = 0;
      break;
    default:
      return NULL;
    }
      
  new_deck->cards = (CARD **) g_malloc (sizeof(CARD *) * 54);
  new_deck->maximum_size = 54;

  if (option == DECK_OPTION_NO_CARD)
    return new_deck;

  k = 0;
  for (i = CLUB; i <= SPADE; i++)
    for (j = ACE; j <= KING; j++)
      new_deck->cards[k++] = card_new (i, j);

  if (option == DECK_OPTION_NO_JOKER)
    return new_deck;

  new_deck->cards[k++] = card_new (0, JOKER);

  if (option == DECK_OPTION_ONE_JOKER)
    return new_deck;

  new_deck->cards[k++] = card_new (1, JOKER);

  return new_deck;
}


/* Destroy the card deck.
   Deallocate memory.  */
void
deck_delete (DECK *deck)
{
  int i;

  for (i = 0; i < deck->cards_number; i++)
    card_delete (deck->cards[i]);

  g_free (deck);
}

/* Return the size of the card deck.  */  
int
deck_number (DECK *deck)
{
  return deck->cards_number;
}


CARD *
deck_view (DECK *deck, int index)
{
  return deck->cards[deck->cards_number - index - 1];
}

CARD *
deck_remove (DECK *deck, int index)
{
  CARD *old_card;
  int i;
  
  old_card = deck->cards[index];
  for (i = index; i < (deck->cards_number - 1); i++)
    deck->cards[i] = deck->cards[i + 1];
  deck->cards_number--;
  return old_card;
}

void
deck_insert (DECK *deck, int index, CARD *card)
{
  int i;
  
  for (i = deck->cards_number; i > index; i--)
    deck->cards[i] = deck->cards[i-1];
  deck->cards[index] = card;
  deck->cards_number++;
}

void
deck_add_top (DECK *deck, CARD *card)
{
  deck_insert (deck, deck_number(deck), card);
}

void
deck_add_bottom (DECK *deck, CARD *card)
{
  deck_insert (deck, 0, card);
}

CARD *
deck_remove_top (DECK *deck)
{
  return deck_remove (deck, deck_number(deck) - 1);
}

CARD *
deck_remove_bottom (DECK *deck)
{
  return deck_remove (deck, 0);
}

CARD *
deck_view_top (DECK *deck)
{
  if (deck_number (deck) == 0)
    return NULL;
  else
    return deck_view (deck, 0);
}

CARD *
deck_view_botom (DECK *deck)
{
  if (deck_number (deck) == 0)
    return NULL;
  else
    return deck_view (deck, deck_number(deck) - 1);
}

void
deck_shuffle (DECK *deck)
{
  DECK *deck_temp;
  int index;
  int number;

  deck_temp = deck_new (DECK_OPTION_NO_CARD);

  while (deck_number(deck) != 0)
    {
      number = deck_number(deck_temp);

      index = (rand ()) % (number + 1);
      
      deck_insert (deck_temp, index, deck_remove_top (deck));
    }  
  
  while (deck_number(deck_temp) != 0)
    {
      number = deck_number(deck);

      index = (rand ()) % (number + 1);
      
      deck_insert (deck, index, deck_remove_top (deck_temp));
    }
  deck_delete(deck_temp);
}


#ifdef DEBUG
void
deck_dump (DECK *deck)
{
  int i;

  if (deck)
    {
      for (i = 0; i < deck_number(deck); i++)
	{
	  card_dump(deck_view(deck,i));
	}
    }
}
#endif /* DEBUG */
