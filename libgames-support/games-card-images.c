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

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "games-card-images.h"
#include "games-card-common.h"
#include "games-preimage.h"

G_DEFINE_TYPE(GamesCardImages, games_card_images, G_TYPE_OBJECT);

GamesCardImages * games_card_images_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_IMAGES, NULL);
}

static void games_card_images_render (GamesCardImages * images, gint cardid)
{
  int sx, sy;
  GdkPixbuf * subpixbuf;

  sx = cardid % 13;
  sy = cardid / 13;

  subpixbuf = gdk_pixbuf_new_subpixbuf (images->source, sx*images->subwidth, 
					sy*images->subheight,
					images->subwidth, images->subheight);
  if (images->prescaled){
    images->pixbufs[cardid]=subpixbuf;
    g_object_ref (images->pixbufs[cardid]);
  } else {
    images->pixbufs[cardid] = gdk_pixbuf_scale_simple (subpixbuf,
                                                       images->width,
                                                       images->height, 
                                                       GDK_INTERP_BILINEAR);
  }
  
  images->rendered[cardid] = TRUE;
  g_object_unref (subpixbuf);
}

static void games_card_images_prerender (GamesCardImages * images)
{
  gchar * fullname;

  /* FIXME: We should search a path. */
  fullname = g_strconcat (CARDDIR, images->themename, NULL);
  if (!images->preimage) {
    images->preimage = games_preimage_new_from_uri (fullname, NULL);
  }

  g_free (fullname);

  if (!images->preimage) {
    g_warning ("Using a fallback card image set.");
    fullname = CARDDIR"bonded.png";
    images->preimage = games_preimage_new_from_uri (fullname, NULL);    
  }

  if (!images->preimage) {
    /* FIXME: Find a better way to report errors. */
    g_warning ("Failed to load the fallback file.\n");
  }

  images->prescaled = games_preimage_is_scalable (images->preimage);
  
  if (!images->prescaled){
    images->source = games_preimage_render_unscaled_pixbuf (images->preimage);
  } else {
    images->source = games_preimage_render (images->preimage, 
                                            images->width*13, 
                                            images->height*5, NULL);
  }

  images->subwidth  = gdk_pixbuf_get_width (images->source)/13;
  images->subheight = gdk_pixbuf_get_height (images->source)/5;

  images->prerendered = TRUE;
}

static void games_card_images_purge (GamesCardImages * images)
{
  int i;

  if (images->prerendered)
    g_object_unref (images->source);

  for (i=0; i<GAMES_CARDS_TOTAL; i++) {
    if (images->rendered[i])
      g_object_unref (images->pixbufs[i]);
    images->rendered[i] = FALSE;
  }
  
  images->prerendered = FALSE;
}

GdkPixbuf * games_card_images_get_card_by_id (GamesCardImages * images,
					      gint cardid)
{
  if (!images->prerendered)
    games_card_images_prerender (images);

  if (!images->rendered[cardid])
    games_card_images_render (images, cardid);

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

  if (images->preimage != NULL){
    g_object_unref (images->preimage);
    images->preimage=NULL;
  }

  games_card_images_purge (images);
}

static void games_card_images_finalize (GamesCardImages * images)
{
  games_card_images_purge (images);
}

static void games_card_images_class_init (GamesCardImagesClass *class)
{
  GObjectClass *oclass = G_OBJECT_CLASS (class);

  oclass->finalize = (GObjectFinalizeFunc) games_card_images_finalize;
}

static void games_card_images_init (GamesCardImages *cardimages)
{
  /* This is the size of the original gdk-card-image cards. */
  cardimages->width = 79;
  cardimages->height = 123;
  cardimages->themename = g_strdup ("bonded.png");

  cardimages->preimage = NULL;
  cardimages->prescaled = FALSE;

  cardimages->prerendered = FALSE;
  cardimages->rendered = g_new0 (gboolean, GAMES_CARDS_TOTAL);
  cardimages->pixbufs = g_new0 (GdkPixbuf *, GAMES_CARDS_TOTAL); 
}
