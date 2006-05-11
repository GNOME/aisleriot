/* AisleRiot - press_data.c
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

#include <gdk/gdk.h>
#include "press_data.h"
#include "card.h"
#include "draw.h"
#include "slot.h"
#include "sol.h"
#include "cscmi.h"
#include "events.h"

press_data_type* press_data; 

void create_press_data (GdkWindow * window)
{
  GdkWindowAttr attributes;

  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = 0;
  attributes.width = card_width;
  attributes.height = card_height;
  attributes.colormap = gdk_drawable_get_colormap (GDK_DRAWABLE(window));
  attributes.visual = gdk_drawable_get_visual (GDK_DRAWABLE(window));
  
  press_data = g_malloc0(sizeof(press_data_type));
  press_data->moving_cards = gdk_window_new(window, &attributes,
					    (GDK_WA_VISUAL | GDK_WA_COLORMAP));
  press_data->highlight_window = gdk_window_new(window, &attributes,
                                                (GDK_WA_VISUAL | GDK_WA_COLORMAP));
  press_data->status = 0;
}

void generate_press_data ( ) {
  GList* tempptr;
  hslot_type hslot = press_data->hslot;
  gint delta, height, width, x, y;
  GdkGC *gc1, *gc2;
  GdkColor masked = {0, 0, 0, 0}, unmasked = {1, 65535, 65535, 65535};
  int num_moving_cards = hslot->length - (press_data->cardid - 1);

  cscmi_record_move(hslot->id, hslot->cards);

  delta = hslot->exposed - num_moving_cards;
  press_data->xoffset -= x = hslot->pixelx + delta * hslot->pixeldx;
  press_data->yoffset -= y = hslot->pixely + delta * hslot->pixeldy;

  press_data->cards = g_list_nth(hslot->cards, press_data->cardid - 1);
  width = card_width + (num_moving_cards - 1) * hslot->pixeldx;
  height = card_height + (num_moving_cards - 1) * hslot->pixeldy;
  press_data->width = width;
  press_data->height = height;

  gdk_window_resize(press_data->moving_cards, width, height);
  gdk_window_move(press_data->moving_cards, x, y);

  press_data->moving_pixmap = 
    gdk_pixmap_new(press_data->moving_cards, width, height,
  		   gdk_drawable_get_visual (press_data->moving_cards)->depth);
  press_data->moving_mask = 
    gdk_pixmap_new (press_data->moving_cards, width, height, 1);

  gc1 = gdk_gc_new (press_data->moving_pixmap);
  gc2 = gdk_gc_new (press_data->moving_mask);

  gdk_gc_set_foreground (gc2, &masked);
  gdk_draw_rectangle (press_data->moving_mask, gc2, TRUE, 0, 0, width, height);
  gdk_gc_set_foreground (gc2, &unmasked);

  gdk_gc_set_clip_mask (gc1, card_mask); 
  gdk_gc_set_clip_mask (gc2, card_mask); 

  x = y = 0; width = card_width; height = card_height;

  for (tempptr = press_data->cards; tempptr; tempptr = tempptr->next) {
    hcard_type hcard = (hcard_type) tempptr->data; 
    GdkPixmap* cardpix;
	 
    if (hcard->direction == UP)
      cardpix = get_card_picture(hcard->suit, hcard->value);
    else
      cardpix = get_card_back_pixmap();

    gdk_gc_set_clip_origin (gc1, x, y);
    gdk_gc_set_clip_origin (gc2, x, y);
    if (cardpix != NULL)
      gdk_draw_drawable (press_data->moving_pixmap, gc1, cardpix,
		       0, 0, x, y, width, height);
    gdk_draw_rectangle (press_data->moving_mask, gc2, TRUE, 
			x, y, width, height);
    
    x += hslot->pixeldx; y += hslot->pixeldy;
  }
  g_object_unref (gc1);
  g_object_unref (gc2);
  
  gdk_window_set_back_pixmap (press_data->moving_cards, 
			      press_data->moving_pixmap, 0);
  gdk_window_shape_combine_mask (press_data->moving_cards, 
				 press_data->moving_mask, 0, 0);
  gdk_window_show (press_data->moving_cards);

  if (press_data->cards->prev) 
    press_data->cards->prev->next = NULL;
  else 
    hslot->cards = NULL;

  press_data->cards->prev = NULL;
  update_slot_length(press_data->hslot);
}

/* This does slightly more than free data, it also hides the window for
 * instance, but it is certainly in the correct spirit. */
void free_press_data(void)
{
  GList *temptr;

  if (press_data == NULL)
    return;

  gdk_window_hide (press_data->moving_cards);
  
  if (press_data->moving_pixmap) {
    g_object_unref (press_data->moving_pixmap);
    press_data->moving_pixmap = NULL;
  }
  if (press_data->moving_mask) {
    g_object_unref (press_data->moving_mask);
    press_data->moving_mask = NULL;
  }
  press_data->status = STATUS_NONE;

  for (temptr = press_data->cards; temptr; temptr = temptr->next) {
    free(temptr->data);
  }
  g_list_free(press_data->cards);
  press_data->cards = NULL;
}
