/* AisleRiot - press_data.c
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

#include <gdk/gdk.h>
#include "press_data.h"
#include "card.h"
#include "draw.h"
#include "slot.h"
#include "sol.h"
#include "cscmi.h"

press_data_type* press_data; 

void generate_press_data ( ) {
  GList* tempptr;
  SCM old_cards = SCM_EOL; 
  hslot_type hslot = press_data->hslot;
  gint delta, height, width, x, y;
  GdkGC *gc1, *gc2;
  GdkColor masked = {0, 0, 0, 0}, unmasked = {1, 65535, 65535, 65535};

  for (tempptr = hslot->cards; tempptr; tempptr = tempptr->next)
    old_cards = gh_cons(make_card(tempptr->data), old_cards);

  delta = hslot->exposed - (hslot->length - press_data->cardid) - 1;
  press_data->xoffset -= x = hslot->x + delta * hslot->dx;
  press_data->yoffset -= y = hslot->y + delta * hslot->dy;

  press_data->cards = g_list_nth(hslot->cards, press_data->cardid - 1);
  width = get_card_width() + (hslot->length - press_data->cardid) * hslot->dx;
  height= get_card_height() + (hslot->length - press_data->cardid) * hslot->dy;
  
  gdk_window_resize(press_data->moving_cards, width, height);
  gdk_window_move(press_data->moving_cards, x, y);

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

  x = y = 0; width = get_card_width(); height = get_card_height();

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
      gdk_draw_pixmap (press_data->moving_pixmap, gc1, cardpix,
		       0, 0, x, y, width, height);
    gdk_draw_rectangle (press_data->moving_mask, gc2, TRUE, 
			x, y, width, height);
    
    x += hslot->dx; y += hslot->dy;
  }
  gdk_gc_destroy (gc1);
  gdk_gc_destroy (gc2);
  
  gdk_window_set_back_pixmap (press_data->moving_cards, 
			      press_data->moving_pixmap, 0);
  gdk_window_shape_combine_mask (press_data->moving_cards, 
				 press_data->moving_mask, 0, 0);
  gdk_window_show (press_data->moving_cards);

  if (press_data->cards->prev) 
    press_data->cards->prev->next = NULL;
  else 
    hslot->cards = NULL;

  gh_call2 (gh_eval_str ("record-move"), gh_long2scm (hslot->id), 
	   old_cards);

  press_data->cards->prev = NULL;
  update_slot_length(press_data->hslot);
}
