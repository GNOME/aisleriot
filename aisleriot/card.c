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
#include "pixmaps/cards.h"
#include "pixmaps/slot.xpm"
#include "pixmaps/back.xpm"
#include "pixmaps/background.xpm"

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


void load_pixmaps(GtkWidget* app) {
  /* cruft */
  mask = gdk_bitmap_create_from_data(app->window, mask_bits, mask_width, mask_height);
  slot_pixmap = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, slot_xpm);
  card_back_pixmap = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, back_xpm);
  default_background_pixmap = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, background_xpm);

  /* cards */
  /* we should put a load bar here... */
  card_pixmaps[0] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _2club_xpm);
  card_pixmaps[1] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _3club_xpm);
  card_pixmaps[2] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _4club_xpm);
  card_pixmaps[3] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _5club_xpm);
  card_pixmaps[4] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _6club_xpm);
  card_pixmaps[5] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _7club_xpm);
  card_pixmaps[6] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _8club_xpm);
  card_pixmaps[7] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _9club_xpm);
  card_pixmaps[8] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _10club_xpm);
  card_pixmaps[9] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, jackclub_xpm);
  card_pixmaps[10] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, queenclub_xpm);
  card_pixmaps[11] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, kingclub_xpm);
  card_pixmaps[12] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, aceclub_xpm);
  card_pixmaps[13] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _2diamond_xpm);
  card_pixmaps[14] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _3diamond_xpm);
  card_pixmaps[15] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _4diamond_xpm);
  card_pixmaps[16] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _5diamond_xpm);
  card_pixmaps[17] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _6diamond_xpm);
  card_pixmaps[18] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _7diamond_xpm);
  card_pixmaps[19] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _8diamond_xpm);
  card_pixmaps[20] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _9diamond_xpm);
  card_pixmaps[21] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _10diamond_xpm);
  card_pixmaps[22] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, jackdiamond_xpm);
  card_pixmaps[23] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, queendiamond_xpm);
  card_pixmaps[24] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, kingdiamond_xpm);
  card_pixmaps[25] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, acediamond_xpm);
  card_pixmaps[26] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _2heart_xpm);
  card_pixmaps[27] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _3heart_xpm);
  card_pixmaps[28] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _4heart_xpm);
  card_pixmaps[29] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _5heart_xpm);
  card_pixmaps[30] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _6heart_xpm);
  card_pixmaps[31] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _7heart_xpm);
  card_pixmaps[32] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _8heart_xpm);
  card_pixmaps[33] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _9heart_xpm);
  card_pixmaps[34] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _10heart_xpm);
  card_pixmaps[35] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, jackheart_xpm);
  card_pixmaps[36] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, queenheart_xpm);
  card_pixmaps[37] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, kingheart_xpm);
  card_pixmaps[38] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, aceheart_xpm);
  card_pixmaps[39] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _2spade_xpm);
  card_pixmaps[40] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _3spade_xpm);
  card_pixmaps[41] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _4spade_xpm);
  card_pixmaps[42] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _5spade_xpm);
  card_pixmaps[43] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _6spade_xpm);
  card_pixmaps[44] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _7spade_xpm);
  card_pixmaps[45] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _8spade_xpm);
  card_pixmaps[46] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _9spade_xpm);
  card_pixmaps[47] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, _10spade_xpm);
  card_pixmaps[48] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, jackspade_xpm);
  card_pixmaps[49] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, queenspade_xpm);
  card_pixmaps[50] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, kingspade_xpm);
  card_pixmaps[51] = gdk_pixmap_create_from_xpm_d(app->window, &mask, NULL, acespade_xpm);
}

void add_card(GList** card_list, hcard_type temp_card) {
  if (*card_list)
	 g_list_append(*card_list, temp_card);
  else {
	 *card_list = g_list_alloc();
	 (*card_list)->data = temp_card;
  }
}
