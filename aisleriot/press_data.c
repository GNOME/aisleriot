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

GdkBitmap *create_back_mask (hslot_type slot, gint width, gint height, gint list_length) {
  gint i;
  GdkBitmap *retval = gdk_pixmap_new (playing_area->window,
				      width,
				      height,
				      1);
  GdkColor color = {0,0,0,0};
  GdkGC *gc = gdk_gc_new (retval);

  gdk_gc_set_foreground (gc, &color);
  gdk_draw_rectangle (retval, gc,
		      TRUE, 0, 0, width, height);
  gdk_gc_set_function (gc, GDK_OR);
  
  for (i = 0; i < list_length; i++)
    gdk_draw_pixmap (retval, gc, mask, 0, 0, 0, i*EXPANDED_VERT_OFFSET, get_card_width(), get_card_height());

  return retval;
}

void generate_press_data(gint x, gint y, gint slotid, gint cardid) {
	GList* tempptr;
	GdkPixmap* tempcard;
	hslot_type slot = get_slot(slotid);
	gint i, tempint;
	GdkGC* gc;
	GdkWindowAttr attributes;

	gc = gdk_gc_new (playing_area->window);
	gdk_gc_set_clip_mask(gc,mask); 

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

	if (slot->type == NORMAL_SLOT) {
		press_data->xoffset = (x - slot->x);
		press_data->yoffset = (y - slot->y);
		press_data->moving_pixmap = 
			gdk_pixmap_new(playing_area->window, 
				       get_card_width(),
				       get_card_height() + (g_list_length(press_data->cards) - 1)*EXPANDED_VERT_OFFSET,
				       gdk_window_get_visual (playing_area->window)->depth);
	}
	else if ((slot->type == EXPANDING_SLOT) || (slot->type == PARTIALLY_EXPANDING_SLOT)) {
		press_data->xoffset = (x - slot->x);
		if ((slot->type == PARTIALLY_EXPANDING_SLOT)
		    && (g_list_length(slot->cards) + g_list_length(press_data->cards) > slot->expansion_depth)) {
			tempint = g_list_length(press_data->cards) - slot->expansion_depth;
			press_data->yoffset = (y - slot->y) + tempint*EXPANDED_VERT_OFFSET;
			if  (tempint == 0)
				slot->expansion_depth = 1;
			else {
				slot->expansion_depth = tempint;
			}
		} 
		else
			press_data->yoffset = (y - slot->y) -(cardid -1)*EXPANDED_VERT_OFFSET;

		press_data->moving_pixmap = 
			gdk_pixmap_new(playing_area->window, 
				       get_card_width(),
				       get_card_height() + (g_list_length(press_data->cards) - 1)*EXPANDED_VERT_OFFSET,
				       gdk_window_get_visual (playing_area->window)->depth);
	}
	else if ((slot->type == EXPANDING_SLOT_RIGHT) || (slot->type == PARTIALLY_EXPANDING_SLOT_RIGHT)) {
		if ((slot->type == PARTIALLY_EXPANDING_SLOT_RIGHT)
		    && (g_list_length(slot->cards) + g_list_length(press_data->cards) > slot->expansion_depth)) {
			tempint = g_list_length(press_data->cards) - slot->expansion_depth;
			press_data->xoffset = (x - slot->x) + tempint*EXPANDED_VERT_OFFSET;

			if  (tempint == 0)
				slot->expansion_depth = 1;
			else 
				slot->expansion_depth = -tempint;
		} 
		else
			press_data->xoffset = (x - slot->x) -(cardid -1)*EXPANDED_HORIZ_OFFSET;
		press_data->yoffset = (y - slot->y);
		press_data->moving_pixmap = 
			gdk_pixmap_new(playing_area->window, 
				       get_card_width() + (g_list_length(press_data->cards) - 1)*EXPANDED_HORIZ_OFFSET,
				       get_card_height(),
				       gdk_window_get_visual (playing_area->window)->depth);
	}

	i = 0;
	for (tempptr = press_data->cards; tempptr; tempptr = tempptr->next, i++) {
		if (((hcard_type) tempptr->data)->direction == UP)
			tempcard = get_card_picture(((hcard_type) tempptr->data)->suit,
						    ((hcard_type) tempptr->data)->value);
		else
			tempcard = get_card_back_pixmap();

		if ((slot->type == EXPANDING_SLOT_RIGHT) || (slot->type == PARTIALLY_EXPANDING_SLOT_RIGHT)) {
			gdk_gc_set_clip_origin(gc,0,0);
			gdk_draw_pixmap(press_data->moving_pixmap,
					gc,
					tempcard,
					0, 0,
					i* EXPANDED_HORIZ_OFFSET, 0,
					-1, -1);
		}
		else{
			gdk_gc_set_clip_origin(gc,0,i * EXPANDED_VERT_OFFSET);
			gdk_draw_pixmap(press_data->moving_pixmap,
					gc,
					tempcard,
					0, 0,
					0, i* EXPANDED_VERT_OFFSET,
					-1, -1);
		}
	}

	if (!press_data->moving_cards) {
	        gint list_length = g_list_length(press_data->cards);
		attributes.window_type = GDK_WINDOW_CHILD;
		attributes.x = x - press_data->xoffset;
		attributes.y = y - press_data->yoffset;
		printf("%d\t%d\n",attributes.x, attributes.y);
		attributes.width = get_card_width();
		attributes.height = get_card_height() + (list_length - 1)*EXPANDED_VERT_OFFSET;
		attributes.wclass = GDK_INPUT_OUTPUT;
		attributes.event_mask = 0;
		attributes.colormap = gdk_window_get_colormap (playing_area->window);
		attributes.visual = gdk_window_get_visual (playing_area->window);
	 
		press_data->moving_cards =  gdk_window_new(playing_area->window,
							   &attributes,
							   (GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP));
		press_data->moving_mask = create_back_mask (slot, attributes.width, attributes.height, list_length);
		gdk_window_set_back_pixmap(press_data->moving_cards, press_data->moving_pixmap, 0);
		gdk_window_shape_combine_mask (press_data->moving_cards, press_data->moving_mask, 0, 0);

		gdk_window_move(press_data->moving_cards,  
				x - press_data->xoffset,
				y - press_data->yoffset);
		gdk_window_show(press_data->moving_cards);
	}
	else {
	        gint list_length = g_list_length(press_data->cards);
		gdk_window_resize(press_data->moving_cards,
				  get_card_width(),
				  get_card_height() + (list_length - 1)*EXPANDED_VERT_OFFSET);
		press_data->moving_mask = create_back_mask (slot,
							    get_card_width(),
							    get_card_height() + (list_length - 1)*EXPANDED_VERT_OFFSET,
							    list_length);
		gdk_window_set_back_pixmap(press_data->moving_cards, press_data->moving_pixmap, 0);
		gdk_window_shape_combine_mask (press_data->moving_cards, press_data->moving_mask, 0, 0);
		gdk_window_move(press_data->moving_cards,  
				x - press_data->xoffset,
				y - press_data->yoffset);
	}
}
