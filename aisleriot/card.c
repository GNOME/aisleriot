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

GdkPixmap *default_background_pixmap; 
GdkPixmap *slot_pixmap;
GdkBitmap *mask;

GdkPixmap* get_card_picture (gint suit, gint rank ) 
{
  return gdk_card_deck_face (GDK_CARD_DECK (card_deck), suit, (rank == 14)?1:rank);
}

GdkPixmap* get_background_pixmap() {
  
  return default_background_pixmap;
}

GdkPixmap* get_slot_pixmap() {
  return slot_pixmap;
}

GdkPixmap* get_card_back_pixmap () {
  return gdk_card_deck_back (GDK_CARD_DECK (card_deck));
}

int get_card_width() {
  int width, height;
  gdk_window_get_size(gdk_card_deck_back (GDK_CARD_DECK (card_deck)), 
		      &width, &height);
  return width;
}

int get_card_height() {
  int width, height;
  gdk_window_get_size(gdk_card_deck_back (GDK_CARD_DECK (card_deck)), 
		      &width, &height);
  return height;
}

int get_horiz_offset() {
  return get_card_width() + x_spacing;
}

int get_vert_offset() {
  return get_card_height() + y_spacing;
}

int get_vert_start() {
  return 30;
}

int get_horiz_start() {
  return 30;
}

GdkPixmap* get_pixmap (const char* filename)
{
  GdkPixmap* ret;
  GdkImlibImage *im;
  char* fullname = gnome_pixmap_file (filename);

  if (fullname == NULL)
    return NULL; 

  im = gdk_imlib_load_image (fullname);
  if (im != NULL) {
    gdk_imlib_render (im, im->rgb_width, im->rgb_height);
    ret = gdk_imlib_copy_image (im);
    gdk_imlib_destroy_image (im);
  } else {
    ret = NULL;
  } 
  g_free (fullname);

  return ret;
}

void load_pixmaps(GtkWidget* app, GdkCardDeckOptions deck_options) 
{
  card_deck = gdk_card_deck_new (app->window, deck_options);
  mask = gdk_card_deck_mask (GDK_CARD_DECK (card_deck)); 
  slot_pixmap = get_pixmap ("cards/slots/plain.png");
  default_background_pixmap = get_pixmap ("cards/baize.png");
}

void free_pixmaps() 
{
  gtk_object_destroy (card_deck);
  if (slot_pixmap != NULL)
    gdk_pixmap_unref (slot_pixmap);
  if (default_background_pixmap != NULL)
    gdk_pixmap_unref (default_background_pixmap);
}

void add_card(GList** card_list, hcard_type temp_card) {
  if (*card_list)
	 g_list_append(*card_list, temp_card);
  else {
	 *card_list = g_list_alloc();
	 (*card_list)->data = temp_card;
  }
}
