/* games-card-images.h
   Copyright 2004 Callum McKenzie

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

/* Manage a set of pixbufs containing a deck of cards. */

#ifndef GAMES_CARD_IMAGES_H
#define GAMES_CARD_IMAGES_H

G_BEGIN_DECLS

#include <gtk/gtk.h>

enum {
  GAMES_CARD_ACE         = 1,
  GAMES_CARD_TWO         = 2,
  GAMES_CARD_THREE       = 3,
  GAMES_CARD_FOUR        = 4,
  GAMES_CARD_FIVE        = 5,
  GAMES_CARD_SIX         = 6,
  GAMES_CARD_SEVEN       = 7,
  GAMES_CARD_EIGHT       = 8,
  GAMES_CARD_NINE        = 9,
  GAMES_CARD_TEN         = 10,
  GAMES_CARD_JACK        = 11,
  GAMES_CARD_QUEEN       = 12,
  GAMES_CARD_KING        = 13,
  GAMES_CARDS_CLUBS      = 0,
  GAMES_CARDS_HEARTS     = 1,
  GAMES_CARDS_DIAMONDS   = 2,
  GAMES_CARDS_SPADES     = 3,
  GAMES_CARD_RED_JOKER   = 52,
  GAMES_CARD_BLACK_JOKER = 53,
  GAMES_CARD_BACK        = 54,
  GAMES_CARDS_TOTAL       = 55
};

typedef struct _GamesCardImages {
  GObject parent;

  gint width;
  gint height;

  gchar * themename;

  gboolean rendered;
  GdkPixbuf ** pixbufs;
} GamesCardImages;

typedef struct _GamesCardImagesClass {
  GObjectClass parent;
} GamesCardImagesClass;

#define GAMES_CARD_IMAGES_TYPE (games_card_images_get_type ())

GType games_card_images_get_type (void);

GamesCardImages * games_card_images_new (void);

/* The real card routine. */
GdkPixbuf * games_card_images_get_card_by_id (GamesCardImages * images,
					      gint cardid);

/* The convenience functions. */
#define GAMES_CARD_ID(suit, rank) ((13*(suit)) + (rank-1))

GdkPixbuf * games_card_images_get_card (GamesCardImages * images, gint suit, 
					gint rank);
GdkPixbuf * games_card_images_get_red_joker (GamesCardImages * images);
GdkPixbuf * games_card_images_get_black_joker (GamesCardImages * images);
GdkPixbuf * games_card_images_get_back (GamesCardImages * images);

void games_card_images_set_size (GamesCardImages * images, 
				 gint width, gint height);
void games_card_images_set_theme (GamesCardImages * images, gchar * name);

G_END_DECLS

#endif /* GAMES_CARD_IMAGES_H */
