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

G_DEFINE_TYPE(GamesCardImages, games_card_images, G_TYPE_OBJECT);

GamesCardImages * games_card_images_new (void)
{
  GamesCardImages * images;

  images = g_object_new (GAMES_CARD_IMAGES_TYPE, NULL);

  /* This is the size of the original gdk-card-images cards. */
  images->width = 79;
  images->height = 123;
  images->themename = g_strdup ("bonded-new.png");

  images->rendered = FALSE;
  images->pixbufs = g_new0 (GdkPixbuf *, GAMES_CARDS_TOTAL);

  return images;
}

static void games_card_images_render (GamesCardImages * images)
{
  int i,j,n;
  GdkPixbuf * source;
  GdkPixbuf * subpixbuf;
  gint subwidth, subheight;
  gchar * fullname;

  /* FIXME: We should search a path. */
  fullname = g_strconcat (DATADIR"/pixmaps/cards/", images->themename, NULL);
  source = gdk_pixbuf_new_from_file (fullname, NULL);
  /* FIXME: Find some way to alert the user of errors. */
  g_free (fullname);

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

static void games_card_images_purge (GamesCardImages * images)
{
  int i;

  for (i=0; i<GAMES_CARDS_TOTAL; i++) {
    if (images->pixbufs[i])
      g_object_unref (images->pixbufs[i]);
  }
  
  images->rendered = FALSE;
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
  if ((width == images->width) && (height == images->height))
    return;

  images->width = width;
  images->height = height;

  games_card_images_purge (images);
}

void games_card_images_set_theme (GamesCardImages * images, gchar * name)
{
  /* Ignore the two obvious problem cases silently. */
  if ((!name) || (*name == '\0'))
    return;

  if (images->themename)
    g_free (images->themename);

  images->themename = g_strdup (name);
  images->rendered = FALSE;

  games_card_images_purge (images);
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
