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

static GdkPixmap *
get_pixmap (char **data)
{
	GdkImlibImage *im;
	GdkPixmap *p;

	im = gdk_imlib_create_image_from_xpm_data (data);
	gdk_imlib_render (im, im->rgb_width, im->rgb_height);
	p = gdk_imlib_copy_image (im);
	gdk_imlib_destroy_image (im);

	return p;
}

void load_pixmaps(GtkWidget* app) {
  /* cruft */
  mask = gdk_bitmap_create_from_data(app->window, mask_bits, mask_width, mask_height);
  slot_pixmap = get_pixmap (slot_xpm);
  card_back_pixmap = get_pixmap (back_xpm);
  default_background_pixmap = get_pixmap (background_xpm);

  /* cards */
  /* we should put a load bar here... */
  card_pixmaps[0] = get_pixmap (_2club_xpm);
  card_pixmaps[1] = get_pixmap (_3club_xpm);
  card_pixmaps[2] = get_pixmap (_4club_xpm);
  card_pixmaps[3] = get_pixmap (_5club_xpm);
  card_pixmaps[4] = get_pixmap (_6club_xpm);
  card_pixmaps[5] = get_pixmap (_7club_xpm);
  card_pixmaps[6] = get_pixmap (_8club_xpm);
  card_pixmaps[7] = get_pixmap (_9club_xpm);
  card_pixmaps[8] = get_pixmap (_10club_xpm);
  card_pixmaps[9] = get_pixmap (jackclub_xpm);
  card_pixmaps[10] = get_pixmap (queenclub_xpm);
  card_pixmaps[11] = get_pixmap (kingclub_xpm);
  card_pixmaps[12] = get_pixmap (aceclub_xpm);
  card_pixmaps[13] = get_pixmap (_2diamond_xpm);
  card_pixmaps[14] = get_pixmap (_3diamond_xpm);
  card_pixmaps[15] = get_pixmap (_4diamond_xpm);
  card_pixmaps[16] = get_pixmap (_5diamond_xpm);
  card_pixmaps[17] = get_pixmap (_6diamond_xpm);
  card_pixmaps[18] = get_pixmap (_7diamond_xpm);
  card_pixmaps[19] = get_pixmap (_8diamond_xpm);
  card_pixmaps[20] = get_pixmap (_9diamond_xpm);
  card_pixmaps[21] = get_pixmap (_10diamond_xpm);
  card_pixmaps[22] = get_pixmap (jackdiamond_xpm);
  card_pixmaps[23] = get_pixmap (queendiamond_xpm);
  card_pixmaps[24] = get_pixmap (kingdiamond_xpm);
  card_pixmaps[25] = get_pixmap (acediamond_xpm);
  card_pixmaps[26] = get_pixmap (_2heart_xpm);
  card_pixmaps[27] = get_pixmap (_3heart_xpm);
  card_pixmaps[28] = get_pixmap (_4heart_xpm);
  card_pixmaps[29] = get_pixmap (_5heart_xpm);
  card_pixmaps[30] = get_pixmap (_6heart_xpm);
  card_pixmaps[31] = get_pixmap (_7heart_xpm);
  card_pixmaps[32] = get_pixmap (_8heart_xpm);
  card_pixmaps[33] = get_pixmap (_9heart_xpm);
  card_pixmaps[34] = get_pixmap (_10heart_xpm);
  card_pixmaps[35] = get_pixmap (jackheart_xpm);
  card_pixmaps[36] = get_pixmap (queenheart_xpm);
  card_pixmaps[37] = get_pixmap (kingheart_xpm);
  card_pixmaps[38] = get_pixmap (aceheart_xpm);
  card_pixmaps[39] = get_pixmap (_2spade_xpm);
  card_pixmaps[40] = get_pixmap (_3spade_xpm);
  card_pixmaps[41] = get_pixmap (_4spade_xpm);
  card_pixmaps[42] = get_pixmap (_5spade_xpm);
  card_pixmaps[43] = get_pixmap (_6spade_xpm);
  card_pixmaps[44] = get_pixmap (_7spade_xpm);
  card_pixmaps[45] = get_pixmap (_8spade_xpm);
  card_pixmaps[46] = get_pixmap (_9spade_xpm);
  card_pixmaps[47] = get_pixmap (_10spade_xpm);
  card_pixmaps[48] = get_pixmap (jackspade_xpm);
  card_pixmaps[49] = get_pixmap (queenspade_xpm);
  card_pixmaps[50] = get_pixmap (kingspade_xpm);
  card_pixmaps[51] = get_pixmap (acespade_xpm);
}

void add_card(GList** card_list, hcard_type temp_card) {
  if (*card_list)
	 g_list_append(*card_list, temp_card);
  else {
	 *card_list = g_list_alloc();
	 (*card_list)->data = temp_card;
  }
}
