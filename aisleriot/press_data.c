/* Aisleriot - press_data.c
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

#define PRESS_DATA_C
#include <gdk/gdk.h>
#include "press_data.h"
#include "card.h"
#include "draw.h"
#include "slot.h"
#include "sol.h"

press_data_type* press_data; 

void generate_press_data(gint x, gint y, gint slotid, gint cardid) {
  GList* tempptr;
  GdkPixmap* tempcard;
  hslot_type slot = get_slot(slotid);
  gint tempint;
  gint length, height, width, dx, dy, x2, y2;
  GdkGC *gc1, *gc2;
  GdkColor masked = {0, 0, 0, 0}, unmasked = {1, 65535, 65535, 65535};
  
  press_data->slot_id = slotid;
  press_data->slot_location = cardid;
  press_data->cards = g_list_nth(slot->cards, cardid - 1);
  press_data->temporary_partial_hack = slot->expansion_depth;
  g_assert(press_data->cards);
  if (press_data->cards->prev) 
    press_data->cards->prev->next = NULL;
  else 
    slot->cards = NULL;
  press_data->cards->prev = NULL;
  update_slot_length(slotid);

  length = g_list_length(press_data->cards);
  
  if (slot->type == NORMAL_SLOT) {
    press_data->xoffset = (x - slot->x);
    press_data->yoffset = (y - slot->y);
    width = get_card_width();
    height = get_card_height() + (length - 1)*EXPANDED_VERT_OFFSET;
    dx = 0; dy = EXPANDED_VERT_OFFSET;
  }
  else if ((slot->type == EXPANDING_SLOT) || 
	   (slot->type == PARTIALLY_EXPANDING_SLOT)) {
    if ((slot->type == PARTIALLY_EXPANDING_SLOT) &&
	(g_list_length(slot->cards) + length > slot->expansion_depth)) {
      tempint = length - slot->expansion_depth;
      press_data->yoffset = (y - slot->y) + tempint*EXPANDED_VERT_OFFSET;
      if  (tempint == 0)
	slot->expansion_depth = 1;
      else {
	slot->expansion_depth = tempint;
      }
    } 
    else
      press_data->yoffset = (y - slot->y) - (cardid - 1)*EXPANDED_VERT_OFFSET;

    press_data->xoffset = (x - slot->x);
    width = get_card_width();
    height = get_card_height() + (length - 1)*EXPANDED_VERT_OFFSET;
    dx = 0; dy = EXPANDED_VERT_OFFSET;
  }
  else if ((slot->type == EXPANDING_SLOT_RIGHT) || 
	   (slot->type == PARTIALLY_EXPANDING_SLOT_RIGHT)) {
    if ((slot->type == PARTIALLY_EXPANDING_SLOT_RIGHT) &&
        (g_list_length(slot->cards) + length > slot->expansion_depth)) {
      tempint = length - slot->expansion_depth;
      press_data->xoffset = (x - slot->x) + tempint*EXPANDED_VERT_OFFSET;
      
      if  (tempint == 0)
	slot->expansion_depth = 1;
      else 
	slot->expansion_depth = -tempint;
    } 
    else
      press_data->xoffset = (x - slot->x) - (cardid - 1)*EXPANDED_HORIZ_OFFSET;

    press_data->yoffset = (y - slot->y);
    height = get_card_height();
    width = get_card_width() + (length - 1)*EXPANDED_HORIZ_OFFSET;
    dx = EXPANDED_HORIZ_OFFSET; dy = 0;
  }
  
  gdk_window_resize(press_data->moving_cards, width, height);

  /* 
   * IMHO gdk could give us better access to the XShape extension
   * All the bitmap stuff would be unneccessary with the ShapeUnion operation
   */
  press_data->moving_pixmap = 
    gdk_pixmap_new(press_data->moving_cards, width, height,
  		   gdk_window_get_visual (press_data->moving_cards)->depth);
  press_data->moving_mask = 
    gdk_pixmap_new (press_data->moving_cards, width, height, 1);

  gc1 = gdk_gc_new (press_data->moving_pixmap);
  gc2 = gdk_gc_new (press_data->moving_mask);

  gdk_gc_set_foreground (gc2, &masked);
  gdk_draw_rectangle (press_data->moving_mask, gc2, TRUE, 0, 0, width, height);
  gdk_gc_set_foreground (gc2, &unmasked);

  gdk_gc_set_clip_mask (gc1, mask); 
  gdk_gc_set_clip_mask (gc2, mask); 

  x2 = y2 = 0;
  width = get_card_width(); height = get_card_height();

  for (tempptr = press_data->cards; tempptr; tempptr = tempptr->next) {
	 
    if (((hcard_type) tempptr->data)->direction == UP)
      tempcard = get_card_picture(((hcard_type) tempptr->data)->suit,
				  ((hcard_type) tempptr->data)->value);
    else
      tempcard = get_card_back_pixmap();

    gdk_gc_set_clip_origin (gc1, x2, y2);
    gdk_gc_set_clip_origin (gc2, x2, y2);
    gdk_draw_pixmap (press_data->moving_pixmap, gc1, tempcard,
		     0, 0, x2, y2, width, height);
    gdk_draw_rectangle (press_data->moving_mask, gc2, TRUE, 
			x2, y2, width, height);
    
    x2 += dx; y2 += dy;
  }
  gdk_gc_destroy(gc1);
  gdk_gc_destroy(gc2);
  
  gdk_window_set_back_pixmap(press_data->moving_cards, 
			     press_data->moving_pixmap, 0);
  gdk_window_shape_combine_mask (press_data->moving_cards, 
				 press_data->moving_mask, 0, 0);
  gdk_window_move(press_data->moving_cards,  
		  x - press_data->xoffset,
		  y - press_data->yoffset);
  gdk_window_show(press_data->moving_cards);
}
