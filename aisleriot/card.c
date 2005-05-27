/* AisleRiot - card.c
 * Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#include "sol.h"
#include "card.h"
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <games-preimage.h>
#include <games-card-pixmaps.h>

GdkPixmap *default_background_pixmap; 
GdkPixmap *droptarget_pixmap;
GamesPreimage *slot_preimage;
GdkPixmap *slot_pixmap = NULL;
GdkBitmap *mask = NULL;
GdkBitmap *slot_mask = NULL;
GdkPixbuf* comppixbuf = NULL;

GamesCardPixmaps * images = NULL;

GdkPixmap* get_card_picture (gint suit, gint rank ) 
{
  if (rank == 0) { /* A joker */
    if (suit == 0) /* A black joker. */
      return games_card_pixmaps_get_black_joker (images);
    else /* A red joker. */
      return games_card_pixmaps_get_red_joker (images);
  }

  return games_card_pixmaps_get_card (images, suit, rank);
}

GdkPixmap* get_droptarget_pixmap (gint suit, gint rank, gboolean direction)
{
  GdkPixbuf* cardpixbuf;
  guint x, y, width, height, rowstride;
  guint16 red, green, blue;
  guchar *pixels, *p;

  if (rank == 0) 
    if (suit == 0) 
      cardpixbuf = games_card_images_get_black_joker (GAMES_CARD_IMAGES(images));
    else 
      cardpixbuf = games_card_images_get_red_joker (GAMES_CARD_IMAGES(images));
  else
    cardpixbuf = games_card_images_get_card (GAMES_CARD_IMAGES(images), suit, rank);

  if (direction)
    cardpixbuf = games_card_images_get_back (GAMES_CARD_IMAGES(images));

  /* FIXME: We can do a color button later... if it's usable. */
  red = 0;
  green = 0;
  blue = 0xAA00 / 0xFF;

  gdk_pixbuf_saturate_and_pixelate (cardpixbuf, comppixbuf, 0, 0);

  pixels = gdk_pixbuf_get_pixels (comppixbuf);
  width = gdk_pixbuf_get_width (comppixbuf);
  height = gdk_pixbuf_get_height (comppixbuf);
  rowstride = gdk_pixbuf_get_rowstride (comppixbuf);

  for (y = 0; y < height; y++) {
    for (x = 0; x < width; x++) {
      p = pixels + y*rowstride + x*4;
      p[0] += (0xFF - p[0]) * red   / 0xFF;
      p[1] += (0xFF - p[1]) * green / 0xFF;
      p[2] += (0xFF - p[2]) * blue  / 0xFF;
    }
  }

  gdk_draw_pixbuf (GDK_DRAWABLE(droptarget_pixmap), NULL, comppixbuf, 0, 0, 0, 0, -1, -1, 0, 0, 0);

  return droptarget_pixmap;
}

GdkPixmap* get_background_pixmap () {
  return default_background_pixmap;
}

GdkPixmap* get_slot_pixmap () {
  return slot_pixmap;
}

GdkPixmap* get_card_back_pixmap () {
  return games_card_pixmaps_get_back (images);
}

static GdkPixmap* get_pixmap (const char* filename)
{
  GdkPixmap* ret;
  GdkPixbuf *im;
  char* fullname = gnome_program_locate_file (NULL,
		  GNOME_FILE_DOMAIN_APP_PIXMAP,
		  filename, TRUE, NULL);

  if (fullname == NULL)
    return NULL; 

  im = gdk_pixbuf_new_from_file (fullname, NULL);
  if (im != NULL) {
    gdk_pixbuf_render_pixmap_and_mask_for_colormap (im, gdk_colormap_get_system(), &ret, NULL, 127);
    gdk_pixbuf_unref (im);
  } else {
    ret = NULL;
  } 
  g_free (fullname);

  return ret;
}

static GamesPreimage* get_preimage (const char* filename)
{
  GamesPreimage *im;
  char* fullname = gnome_program_locate_file (NULL,
                                              GNOME_FILE_DOMAIN_APP_PIXMAP,
                                              filename, TRUE, NULL);

  if (fullname == NULL)
    return NULL; 

  im = games_preimage_new_from_file (fullname, NULL);
  g_free (fullname);

  return im;
}

void load_pixmaps (void) 
{
  slot_preimage = get_preimage ("cards/slot.svg");
  default_background_pixmap = get_pixmap ("cards/baize.png");
}

void free_pixmaps () 
{
  if (slot_preimage != NULL)
    g_object_unref (slot_preimage);
  if (default_background_pixmap != NULL)
    g_object_unref (default_background_pixmap);
}

void add_card(GList** card_list, hcard_type temp_card) {
  if (*card_list)
	 g_list_append(*card_list, temp_card);
  else {
	 *card_list = g_list_alloc();
	 (*card_list)->data = temp_card;
  }
}

void set_card_size (gint width, gint height)
{
  GdkPixbuf *scaled;

  if (slot_pixmap)
    g_object_unref (slot_pixmap);
  if (slot_mask)
    g_object_unref (slot_mask);
  if (droptarget_pixmap)
    g_object_unref (droptarget_pixmap);
  if (comppixbuf) {
    g_object_unref (comppixbuf);
  }

  scaled = games_preimage_render (slot_preimage, width, height, 
				  NULL);

  gdk_pixbuf_render_pixmap_and_mask_for_colormap (scaled, gdk_colormap_get_system(), &slot_pixmap, &slot_mask, 255);

  gdk_gc_set_clip_mask (slot_gc, slot_mask); 

  g_object_unref (scaled);

  if (!images) {
    images = games_card_pixmaps_new (playing_area->window);
    games_card_pixmaps_set_theme (images, card_style); 
  }

  games_card_pixmaps_set_size (images, width, height);  

  mask = games_card_pixmaps_get_mask (images);
  gdk_gc_set_clip_mask (draw_gc, mask);

  comppixbuf = gdk_pixbuf_copy (games_card_images_get_back (GAMES_CARD_IMAGES(images)));
  droptarget_pixmap = gdk_pixmap_new (GDK_DRAWABLE(playing_area->window), width, height, -1);
}

void set_card_theme (gchar * theme)
{
  games_card_pixmaps_set_theme (images, theme);
  mask = games_card_pixmaps_get_mask (images);
  gdk_gc_set_clip_mask (draw_gc, mask);  
}
