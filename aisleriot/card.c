/* Aisleriot - card.c
 * Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
 *
 * This game is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#define CARD_C

#include <gdk_imlib.h>

#include "card.h"
#include "pixmaps/mask.xbm"

GdkPixmap *default_background_pixmap; 
GdkPixmap *slot_pixmap;
GdkPixmap *card_back_pixmap;
GdkPixmap *card_pixmaps[52];
GdkBitmap *mask;

GdkPixmap* get_card_picture(gint suit, gint value ) {
  if (value == 1) 
	 return card_pixmaps[suit * 13 + value + 11 ];  

  return card_pixmaps[suit * 13 + value - 2 ];  
}


GdkPixmap* get_background_pixmap() {
  
  return default_background_pixmap;
}

GdkPixmap* get_slot_pixmap() {
  return slot_pixmap;
}

void load_file_to_pixmap (char *filename, GdkPixmap **p, void *data)
{
	char *name = g_copy_strings ("pixmaps/cards/", filename, NULL);
	char *full = gnome_unconditional_datadir_file (name);
	
	gdk_imlib_load_file_to_pixmap (full, p, data);
	g_free (full);
	g_free (name);
}

void load_pixmaps(GtkWidget* app) {
  GtkStyle* style;
  /* change these all to not load from file! */
	 
  style = gtk_widget_get_style(app);
  mask = gdk_bitmap_create_from_data(app->window, mask_bits, mask_width, mask_height);
  load_file_to_pixmap("slot.xpm", &slot_pixmap, NULL);
  load_file_to_pixmap("back.xpm", &card_back_pixmap, NULL);
  load_file_to_pixmap("background.xpm", &default_background_pixmap, NULL);
  load_file_to_pixmap("2club.xpm", &card_pixmaps[0], NULL);
  load_file_to_pixmap("3club.xpm", &card_pixmaps[1], NULL);
  load_file_to_pixmap("4club.xpm", &card_pixmaps[2], NULL);
  load_file_to_pixmap("5club.xpm", &card_pixmaps[3], NULL);
  load_file_to_pixmap("6club.xpm", &card_pixmaps[4], NULL);
  load_file_to_pixmap("7club.xpm", &card_pixmaps[5], NULL);
  load_file_to_pixmap("8club.xpm", &card_pixmaps[6], NULL);
  load_file_to_pixmap("9club.xpm", &card_pixmaps[7], NULL);
  load_file_to_pixmap("10club.xpm", &card_pixmaps[8], NULL);
  load_file_to_pixmap("jackclub.xpm", &card_pixmaps[9], NULL);
  load_file_to_pixmap("queenclub.xpm", &card_pixmaps[10], NULL);
  load_file_to_pixmap("kingclub.xpm", &card_pixmaps[11], NULL);
  load_file_to_pixmap("aceclub.xpm", &card_pixmaps[12], NULL);
  load_file_to_pixmap("2diamond.xpm", &card_pixmaps[13], NULL);
  load_file_to_pixmap("3diamond.xpm", &card_pixmaps[14], NULL);
  load_file_to_pixmap("4diamond.xpm", &card_pixmaps[15], NULL);
  load_file_to_pixmap("5diamond.xpm", &card_pixmaps[16], NULL);
  load_file_to_pixmap("6diamond.xpm", &card_pixmaps[17], NULL);
  load_file_to_pixmap("7diamond.xpm", &card_pixmaps[18], NULL);
  load_file_to_pixmap("8diamond.xpm", &card_pixmaps[19], NULL);
  load_file_to_pixmap("9diamond.xpm", &card_pixmaps[20], NULL);
  load_file_to_pixmap("10diamond.xpm", &card_pixmaps[21], NULL);
  load_file_to_pixmap("jackdiamond.xpm", &card_pixmaps[22], NULL);
  load_file_to_pixmap("queendiamond.xpm", &card_pixmaps[23], NULL);
  load_file_to_pixmap("kingdiamond.xpm", &card_pixmaps[24], NULL);
  load_file_to_pixmap("acediamond.xpm", &card_pixmaps[25], NULL);
  load_file_to_pixmap("2heart.xpm", &card_pixmaps[26], NULL);
  load_file_to_pixmap("3heart.xpm", &card_pixmaps[27], NULL);
  load_file_to_pixmap("4heart.xpm", &card_pixmaps[28], NULL);
  load_file_to_pixmap("5heart.xpm", &card_pixmaps[29], NULL);
  load_file_to_pixmap("6heart.xpm", &card_pixmaps[30], NULL);
  load_file_to_pixmap("7heart.xpm", &card_pixmaps[31], NULL);
  load_file_to_pixmap("8heart.xpm", &card_pixmaps[32], NULL);
  load_file_to_pixmap("9heart.xpm", &card_pixmaps[33], NULL);
  load_file_to_pixmap("10heart.xpm", &card_pixmaps[34], NULL);
  load_file_to_pixmap("jackheart.xpm", &card_pixmaps[35], NULL);
  load_file_to_pixmap("queenheart.xpm", &card_pixmaps[36], NULL);
  load_file_to_pixmap("kingheart.xpm", &card_pixmaps[37], NULL);
  load_file_to_pixmap("aceheart.xpm", &card_pixmaps[38], NULL);
  load_file_to_pixmap("2spade.xpm", &card_pixmaps[39], NULL);
  load_file_to_pixmap("3spade.xpm", &card_pixmaps[40], NULL);
  load_file_to_pixmap("4spade.xpm", &card_pixmaps[41], NULL);
  load_file_to_pixmap("5spade.xpm", &card_pixmaps[42], NULL);
  load_file_to_pixmap("6spade.xpm", &card_pixmaps[43], NULL);
  load_file_to_pixmap("7spade.xpm", &card_pixmaps[44], NULL);
  load_file_to_pixmap("8spade.xpm", &card_pixmaps[45], NULL);
  load_file_to_pixmap("9spade.xpm", &card_pixmaps[46], NULL);
  load_file_to_pixmap("10spade.xpm", &card_pixmaps[47], NULL);
  load_file_to_pixmap("jackspade.xpm", &card_pixmaps[48], NULL);
  load_file_to_pixmap("queenspade.xpm", &card_pixmaps[49], NULL);
  load_file_to_pixmap("kingspade.xpm", &card_pixmaps[50], NULL);
  load_file_to_pixmap("acespade.xpm", &card_pixmaps[51], NULL);
}

GdkPixmap* get_card_back_pixmap() {
  return card_back_pixmap;
}

int get_card_width() {
  int width, height;
  gdk_window_get_size(card_back_pixmap, &width, &height);
  return width;
}
int get_card_height() {
  int width, height;
  gdk_window_get_size(card_back_pixmap, &width, &height);
  return height;
}
int get_horiz_offset() {
  return get_card_width() + HORIZ_SPACING;
}
int get_vert_offset() {
  return get_card_height() + VERT_SPACING;
}
int get_vert_start() {
  return 30;
}
int get_horiz_start() {
  return 30;
}

void add_card(GList** card_list, hcard_type temp_card) {
  if (*card_list)
	 g_list_append(*card_list, temp_card);
  else {
	 *card_list = g_list_alloc();
	 (*card_list)->data = temp_card;
  }

}
