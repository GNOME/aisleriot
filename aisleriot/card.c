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
#include <games-card-pixmaps.h>

GdkPixmap *default_background_pixmap; 
GdkPixbuf *slot_pixbuf;
GdkPixmap *slot_pixmap = NULL;
GdkBitmap *mask = NULL;
GdkBitmap *slot_mask = NULL;

GamesCardPixmaps * images = NULL;

GdkPixmap* get_card_picture (gint suit, gint rank ) 
{
  return games_card_pixmaps_get_card (images, suit, rank);
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
    gdk_pixbuf_render_pixmap_and_mask (im, &ret, NULL, 127);
    gdk_pixbuf_unref (im);
  } else {
    ret = NULL;
  } 
  g_free (fullname);

  return ret;
}

static GdkPixbuf* get_pixbuf (const char* filename)
{
  GdkPixbuf *im;
  char* fullname = gnome_program_locate_file (NULL,
                                              GNOME_FILE_DOMAIN_APP_PIXMAP,
                                              filename, TRUE, NULL);

  if (fullname == NULL)
    return NULL; 

  im = gdk_pixbuf_new_from_file (fullname, NULL);
  g_free (fullname);

  return im;
}

void load_pixmaps (void) 
{
  slot_pixbuf = get_pixbuf ("cards/slots/plain.png");
  default_background_pixmap = get_pixmap ("cards/baize.png");
}

void free_pixmaps () 
{
  if (slot_pixbuf != NULL)
    gdk_pixbuf_unref (slot_pixbuf);
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

  scaled = gdk_pixbuf_scale_simple (slot_pixbuf, width, height, 
				    GDK_INTERP_BILINEAR);

  gdk_pixbuf_render_pixmap_and_mask (scaled, &slot_pixmap, &slot_mask, 255);

  gdk_gc_set_clip_mask (slot_gc, slot_mask); 

  g_object_unref (scaled);

  if (!images) {
    images = games_card_pixmaps_new (playing_area->window);
    games_card_pixmaps_set_theme (images, card_style); 
  }
  games_card_pixmaps_set_size (images, width, height);  
  mask = games_card_pixmaps_get_mask (images);
  gdk_gc_set_clip_mask (draw_gc, mask);
}

void set_card_theme (gchar * theme)
{
  games_card_pixmaps_set_theme (images, theme);
  mask = games_card_pixmaps_get_mask (images);
  gdk_gc_set_clip_mask (draw_gc, mask);  
}
