/* games-card-pixmaps.c
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

/* Manage a set of pixmaps of card images. This is a subclass of
   games-card-images. This is disjoint from games-card-images to
   separate out the X-server dependent portions. You should only need
   to use one of the two depending on whether you need the pixbufs or
   the pixmaps.*/

#include <gtk/gtk.h>

#include "games-card-images.h"

#include "games-card-pixmaps.h"

G_DEFINE_TYPE(GamesCardPixmaps, games_card_pixmaps, GAMES_TYPE_CARD_IMAGES);

GamesCardPixmaps * games_card_pixmaps_new (GdkWindow * drawable)
{
  GamesCardPixmaps * images;

  images = g_object_new (GAMES_TYPE_CARD_PIXMAPS, NULL);

  images->drawable = drawable;

  return images;
}

GdkPixmap * games_card_pixmaps_get_card_by_id (GamesCardPixmaps * images,
					      gint cardid)
{
  GdkPixbuf * pixbuf;
  gint width, height;

  g_return_val_if_fail (((cardid >= 0) && (cardid < GAMES_CARDS_TOTAL)), NULL);

  if (images->pixmaps[cardid])
    return images->pixmaps[cardid];

  pixbuf = games_card_images_get_card_by_id (GAMES_CARD_IMAGES (images), cardid);
  width = images->parent.width;
  height = images->parent.height;

  if (images->mask) {
    images->pixmaps[cardid] = gdk_pixmap_new (images->drawable, width, height, -1);
    gdk_draw_pixbuf (images->pixmaps[cardid], NULL, pixbuf, 0, 0, 0, 0, width, height, 
		     GDK_RGB_DITHER_NORMAL, 0, 0);
  } else {
    gdk_pixbuf_render_pixmap_and_mask_for_colormap (pixbuf, 
						    gdk_colormap_get_system(),
						    &(images->pixmaps[cardid]), &(images->mask), 127);
  }

  return images->pixmaps[cardid];
}

GdkBitmap * games_card_pixmaps_get_mask (GamesCardPixmaps * images)
{
  if (!images->mask) {
    /* Force the mask to be drawn and choose the most likely card to be drawn
     * while were at it. */
    games_card_pixmaps_get_back (images);
  }

  return images->mask;
}

GdkPixmap * games_card_pixmaps_get_card (GamesCardPixmaps * images, gint suit, 
					gint rank)
{
  g_return_val_if_fail (((suit >= GAMES_CARDS_CLUBS) && 
		         (suit <= GAMES_CARDS_SPADES)),
			NULL);
  g_return_val_if_fail (((rank >= GAMES_CARD_ACE) && 
		         (rank <= GAMES_CARD_ACE_HIGH)),
			NULL);

  return games_card_pixmaps_get_card_by_id (images, GAMES_CARD_ID(suit, rank));
}

GdkPixmap * games_card_pixmaps_get_red_joker (GamesCardPixmaps * images)
{
  return games_card_pixmaps_get_card_by_id (images, GAMES_CARD_BACK);
}

GdkPixmap * games_card_pixmaps_get_black_joker (GamesCardPixmaps * images)
{
  return games_card_pixmaps_get_card_by_id (images, GAMES_CARD_BLACK_JOKER);
}

GdkPixmap * games_card_pixmaps_get_back (GamesCardPixmaps * images)
{
  return games_card_pixmaps_get_card_by_id (images, GAMES_CARD_BACK);
}

static void games_card_pixmaps_purge (GamesCardPixmaps * images)
{
  int i;

  for (i=0; i<GAMES_CARDS_TOTAL; i++) {
    if (images->pixmaps[i]) {
      g_object_unref (images->pixmaps[i]);
      images->pixmaps[i] = NULL;
    }
  }

  if (images->mask) {
    g_object_unref (images->mask);
    images->mask = NULL;
  }
}

void games_card_pixmaps_set_size (GamesCardPixmaps * images, 
				 gint width, gint height)
{
  games_card_images_set_size (&images->parent, width, height);
  games_card_pixmaps_purge (images);
}

void games_card_pixmaps_set_theme (GamesCardPixmaps * images, gchar * name)
{
  games_card_images_set_theme (&images->parent, name);
  games_card_pixmaps_purge (images);
}

static void games_card_pixmaps_finalize (GamesCardPixmaps * images)
{
  games_card_pixmaps_purge (images);
}

static void games_card_pixmaps_class_init (GamesCardPixmapsClass *class)
{
  GObjectClass *oclass = G_OBJECT_CLASS (class);

  oclass->finalize = (GObjectFinalizeFunc) games_card_pixmaps_finalize;
}

static void games_card_pixmaps_init (GamesCardPixmaps *cardimages)
{
  cardimages->pixmaps = g_new0 (GdkPixmap *, GAMES_CARDS_TOTAL);
  cardimages->mask = NULL;
}
