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

#ifndef GAMES_CARD_PIXMAPS_H
#define GAMES_CARD_PIXMAPS_H

G_BEGIN_DECLS

#include <gtk/gtk.h>

#include <games-card-images.h>

typedef struct _GamesCardPixmaps {
  GamesCardImages parent;

  GdkPixmap ** pixmaps;
  GdkBitmap * mask;
  GdkDrawable * drawable;
} GamesCardPixmaps;

typedef struct _GamesCardPixmapsClass {
  GamesCardImagesClass parent;
} GamesCardPixmapsClass;

#define GAMES_TYPE_CARD_PIXMAPS (games_card_pixmaps_get_type ())
#define GAMES_CARD_PIXMAPS(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_PIXMAPS, GamesCardImages))
#define GAMES_CARD_PIXMAPS_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_PIXMAPS, GamesCardImagesClass))
#define GAMES_IS_CARD_PIXMAPS(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_PIXMAPS))
#define GAMES_IS_CARD_PIXMAPS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_PIXMAPS))
#define GAMES_CARD_PIXMAPS_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_PIXMAPS))

GType games_card_pixmaps_get_type (void);

GamesCardPixmaps * games_card_pixmaps_new (GdkWindow * drawable);

/* The real card routine. */
GdkPixmap * games_card_pixmaps_get_card_by_id (GamesCardPixmaps * images,
					      gint cardid);

GdkBitmap * games_card_pixmaps_get_mask (GamesCardPixmaps * images);

/* The convenience functions. */
#define GAMES_CARD_ID(suit, rank) ((13*(suit)) + (rank-1))

GdkPixmap * games_card_pixmaps_get_card (GamesCardPixmaps * images, gint suit, 
					gint rank);
GdkPixmap * games_card_pixmaps_get_red_joker (GamesCardPixmaps * images);
GdkPixmap * games_card_pixmaps_get_black_joker (GamesCardPixmaps * images);
GdkPixmap * games_card_pixmaps_get_back (GamesCardPixmaps * images);

void games_card_pixmaps_set_size (GamesCardPixmaps * images, 
				 gint width, gint height);
void games_card_pixmaps_set_theme (GamesCardPixmaps * images, gchar * name);

G_END_DECLS

#endif /* GAMES_CARD_PIXMAPS_H */
