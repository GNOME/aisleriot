/* AisleRiot - draw.c
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

#include <glib.h>
#include <math.h>

#include "draw.h"
#include "sol.h"
#include "slot.h"
#include "card.h"

/* The size of the drawing area. */
int window_width = 0;
int window_height = 0;

double xslotstep = 0;
double yslotstep = 0;

static double width = 0.0;
static double height = 0.0;

static void calculate_card_location (hslot_type hslot)
{
  hslot->pixelx = xslotstep*(hslot->x + 0.5) - get_card_width () / 2;
  hslot->pixely = yslotstep*(hslot->y + 0.5) - get_card_height () / 2;
}

/* Work out new sizes and spacings for the cards. */
void set_geometry (double new_width, double new_height) {
  width = new_width;
  height = new_height;

  xslotstep = 1.0*window_width/width;
  yslotstep = 1.0*window_height/height;

  /* FIXME: Resize the cards in here. */

  /* Recalculate the slot locations. */
  g_list_foreach (get_slot_list (), (GFunc) calculate_card_location, NULL);
}

void rescale_cards (void) {
  set_geometry (width, height);
}

void draw_cards () {
  GList* slot;
  gint x, y;
  GList* card_list;
  GdkPixmap *image;

  gdk_gc_set_clip_mask (draw_gc,mask); 
  
  for (slot = get_slot_list (); slot; slot = slot->next) {
    hslot_type hslot = (hslot_type)slot->data;

    if ((card_list = hslot->cards)) {

      card_list = g_list_nth (card_list, hslot->length - hslot->exposed);

      x = hslot->pixelx;
      y = hslot->pixely;

      for (; card_list; card_list = card_list->next) {
	card_type *card = card_list->data;

	if (card->direction == DOWN) 
	  image = get_card_back_pixmap ();
	else 
	  image = get_card_picture (card->suit, card->value);
	
	gdk_gc_set_clip_origin (draw_gc, x, y);
	if (image != NULL)
	  gdk_draw_drawable (surface, draw_gc, image, 0, 0, x, y, -1, -1);
	
	x += hslot->dx; y += hslot->dy;
      }
    }
  }
  gdk_gc_set_clip_mask (draw_gc,NULL); 
}

void take_snapshot() {
  GList* slot;

  gdk_draw_rectangle (surface, draw_gc, TRUE, 0, 0, -1, -1);

  for (slot = get_slot_list (); slot; slot = slot->next) {
    GdkPixbuf *slot_pixbuf;

    slot_pixbuf = get_slot_pixbuf ();
    if (slot_pixbuf != NULL)
      gdk_draw_pixbuf (surface, draw_gc,
		       slot_pixbuf, 0, 0, 
		       ((hslot_type)slot->data)->pixelx,
		       ((hslot_type)slot->data)->pixely,
		       -1, -1, GDK_RGB_DITHER_MAX,
                       0, 0);
  }
  draw_cards ();
  gdk_window_set_back_pixmap (playing_area->window, surface, 0);
}

void refresh_screen () {
  take_snapshot ();
  gdk_window_clear (playing_area->window);
}
