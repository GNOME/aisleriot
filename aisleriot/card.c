/* AisleRiot - card.c
 * Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
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

GdkPixmap *default_background_pixmap; 
GdkPixbuf *slot_pixbuf;
GdkBitmap *mask;

GdkPixmap* get_card_picture (gint suit, gint rank ) 
{
  return gdk_card_deck_face (GDK_CARD_DECK (card_deck), suit, (rank == 14)?1:rank);
}

GdkPixmap* get_background_pixmap () {
  
  return default_background_pixmap;
}

GdkPixbuf* get_slot_pixbuf () {
  return slot_pixbuf;
}

GdkPixmap* get_card_back_pixmap () {
  return gdk_card_deck_back (GDK_CARD_DECK (card_deck));
}

int get_card_width () {
  int width, height;
  gdk_drawable_get_size(gdk_card_deck_back (GDK_CARD_DECK (card_deck)), 
		      &width, &height);
  return width;
}

int get_card_height () {
  int width, height;
  gdk_drawable_get_size(gdk_card_deck_back (GDK_CARD_DECK (card_deck)), 
		      &width, &height);
  return height;
}

int get_horiz_offset () {
  return get_card_width() + x_spacing;
}

int get_vert_offset () {
  return get_card_height() + y_spacing;
}

int get_vert_start () {
  return 30;
}

int get_horiz_start () {
  return 30;
}

GdkPixmap* get_pixmap (const char* filename)
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

GdkPixbuf* get_pixbuf (const char* filename)
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

void load_pixmaps (GtkWidget* app, GdkCardDeckOptions deck_options) 
{
  card_deck = gdk_card_deck_new (app->window, deck_options);
  mask = gdk_card_deck_mask (GDK_CARD_DECK (card_deck)); 
  slot_pixbuf = get_pixbuf ("cards/slots/plain.png");
  default_background_pixmap = get_pixmap ("cards/baize.png");
}

void free_pixmaps () 
{
  gtk_object_destroy (card_deck);
  if (slot_pixbuf != NULL)
    gdk_pixbuf_unref (slot_pixbuf);
  if (default_background_pixmap != NULL)
    gdk_drawable_unref (default_background_pixmap);
}

void add_card(GList** card_list, hcard_type temp_card) {
  if (*card_list)
	 g_list_append(*card_list, temp_card);
  else {
	 *card_list = g_list_alloc();
	 (*card_list)->data = temp_card;
  }
}
