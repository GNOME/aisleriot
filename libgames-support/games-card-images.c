/* games-card-images.c
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

#include <gtk/gtk.h>

#include "games-card-images.h"

GamesCardImages * games_card_images_new_with_size (gint width, gint height)
{
  GamesCardImages * images;

  images = g_object_new (GAMES_CARD_IMAGES_TYPE, NULL);

  images->width = width;
  images->height = height;

  images->rendered = FALSE;
  images->pixbufs = g_new0 (GdkPixbuf *, GAMES_CARDS_TOTAL);

  return images;
}

GamesCardImages * games_card_images_new (void)
{
  return games_card_images_new_with_size (0, 0);
}

static void games_card_images_render (GamesCardImages * images)
{
  int i,j,n;
  GdkPixbuf * source;
  GdkPixbuf * subpixbuf;
  gint subwidth, subheight;

  /* FIXME: This should not be hard-coded. */
  source = gdk_pixbuf_new_from_file (DATADIR"/pixmaps/cards/bonded-new.png", NULL);
  
  subwidth = gdk_pixbuf_get_width (source)/13;
  subheight = gdk_pixbuf_get_height (source)/5;

  n = 0;
  for (i=0; i<4; i++) {
    for (j=0; j<13; j++) {
      subpixbuf = gdk_pixbuf_new_subpixbuf (source, j*subwidth, i*subheight,
					    subwidth, subheight);
      images->pixbufs[n] = gdk_pixbuf_scale_simple (subpixbuf, images->width, 
						    images->height,  
						    GDK_INTERP_BILINEAR);
      g_object_unref (subpixbuf);
      n++;
    }
  }
  subpixbuf = gdk_pixbuf_new_subpixbuf (source, 0, 4*subheight,
					subwidth, subheight);
  images->pixbufs[GAMES_CARD_BLACK_JOKER] = gdk_pixbuf_scale_simple (subpixbuf,
								     images->width, 
								     images->height,  
								     GDK_INTERP_BILINEAR);
  g_object_unref (subpixbuf);
  subpixbuf = gdk_pixbuf_new_subpixbuf (source, subwidth, 4*subheight,
					subwidth, subheight);
  images->pixbufs[GAMES_CARD_RED_JOKER] = gdk_pixbuf_scale_simple (subpixbuf,
								   images->width, 
								   images->height,  
								   GDK_INTERP_BILINEAR);
  g_object_unref (subpixbuf);
  subpixbuf = gdk_pixbuf_new_subpixbuf (source, 2*subwidth, 4*subheight,
					subwidth, subheight);
  images->pixbufs[GAMES_CARD_BACK] = gdk_pixbuf_scale_simple (subpixbuf,
							      images->width, 
							      images->height,  
							      GDK_INTERP_BILINEAR);
  g_object_unref (subpixbuf);

  images->rendered = TRUE;

  g_object_unref (source);
}

GdkPixbuf * games_card_images_get_card_by_id (GamesCardImages * images,
					      gint cardid)
{
  if (!images->rendered)
    games_card_images_render (images);

  return images->pixbufs[cardid];
}

GdkPixbuf * games_card_images_get_card (GamesCardImages * images, gint suit,
					gint rank)
{
  return games_card_images_get_card_by_id (images, GAMES_CARD_ID(suit, rank));
}

GdkPixbuf * games_card_images_get_back (GamesCardImages * images)
{
  return games_card_images_get_card_by_id (images, GAMES_CARD_BACK);
}

GdkPixbuf * games_card_images_get_red_joker (GamesCardImages * images)
{
  return games_card_images_get_card_by_id (images, GAMES_CARD_RED_JOKER);
}

GdkPixbuf * games_card_images_get_black_joker (GamesCardImages * images)
{
  return games_card_images_get_card_by_id (images, GAMES_CARD_BLACK_JOKER);
}

void games_card_images_set_size (GamesCardImages * images, 
				 gint width, gint height)
{
  int i;

  if ((width == images->width) && (height == images->height))
    return;

  images->width = width;
  images->height = height;

  for (i=0; i<GAMES_CARDS_TOTAL; i++) {
    if (images->pixbufs[i])
      g_object_unref (images->pixbufs[i]);
  }
  
  images->rendered = FALSE;
}

static void games_card_images_finalize (GamesCardImages * images)
{
  int i;

  for (i=0; i<GAMES_CARDS_TOTAL; i++) {
    if (images->pixbufs[i])
      g_object_unref (images->pixbufs[i]);
  } 
}

static void games_card_images_class_init (GamesCardImagesClass *class)
{
  GObjectClass *oclass = G_OBJECT_CLASS (class);

  oclass->finalize = (GObjectFinalizeFunc) games_card_images_finalize;
}

static void games_card_images_init (GamesCardImages *cardimages)
{
 
}

GType games_card_images_get_type (void)
{
  static GType type = 0;
  static const GTypeInfo info = {
    sizeof (GamesCardImagesClass),
    NULL,
    NULL,
    (GClassInitFunc) games_card_images_class_init,
    NULL,
    NULL,
    sizeof (GamesCardImages),
    0,     
    (GInstanceInitFunc) games_card_images_init
  };

  if (!type)
    type = g_type_register_static (G_TYPE_OBJECT, "GamesCardImages", &info, 0);

  return type;
}
