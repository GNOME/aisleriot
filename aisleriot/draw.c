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

#include "draw.h"
#include "sol.h"
#include "slot.h"
#include "card.h"

void draw_cards () {
  GList* slot;
  gint x, y;
  GList* card_list;
  GdkPixmap *image;

  gdk_gc_set_clip_mask(draw_gc,mask); 
  
  for (slot = get_slot_list(); slot; slot = slot->next) {
    hslot_type hslot = (hslot_type)slot->data;

    if ((card_list = hslot->cards)) {

      card_list = g_list_nth(card_list, hslot->length - hslot->exposed);

      x = hslot->x;
      y = hslot->y;

      for (; card_list; card_list = card_list->next) {
	card_type *card = card_list->data;

	if (card->direction == DOWN) 
	  image = get_card_back_pixmap();
	else 
	  image = get_card_picture(card->suit, card->value);
	
	gdk_gc_set_clip_origin(draw_gc, x, y);
	if (image != NULL)
	  gdk_draw_pixmap(surface, draw_gc, image, 0, 0, x, y, -1, -1);
	
	x += hslot->dx; y += hslot->dy;
      }
    }
  }
  gdk_gc_set_clip_mask(draw_gc,NULL); 
}

void take_snapshot() {
  GList* slot;

  gdk_draw_rectangle (surface, draw_gc, TRUE, 0, 0, -1, -1);

  for (slot = get_slot_list(); slot; slot = slot->next) {
    GdkPixmap *slot_pixmap;
    gdk_gc_set_clip_mask(draw_gc,mask); 
    gdk_gc_set_clip_origin(draw_gc, ((hslot_type)slot->data)->x, 
			   ((hslot_type)slot->data)->y);
    slot_pixmap = get_slot_pixmap();
    if (slot_pixmap != NULL)
      gdk_draw_pixmap (surface, draw_gc,
		       slot_pixmap, 0, 0, 
		       ((hslot_type)slot->data)->x, 
		       ((hslot_type)slot->data)->y, -1, -1);
    gdk_gc_set_clip_mask(draw_gc,NULL); 
  }
  draw_cards();
  gdk_window_set_back_pixmap(playing_area->window, surface, 0);
}

void refresh_screen() {
  take_snapshot();
  gdk_window_clear(playing_area->window);
}
