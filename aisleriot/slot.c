/* AisleRiot - slot.c
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

#include <stdlib.h>

#include "slot.h"
#include "card.h"

GList *slot_list = NULL;

void slot_pressed(gint x, gint y, hslot_type *slot, gint *cardid) {
  GList *tempptr;

  gint num_cards;
  gboolean got_slot = FALSE;

  *slot = NULL;
  *cardid = -1;

  for (tempptr = g_list_last(slot_list); tempptr; tempptr = tempptr->prev) {

    hslot_type hslot = (hslot_type) tempptr->data;

    /* if point is within our rectangle */
    if (hslot->x < x && x < hslot->x + hslot->width && 
	hslot->y < y && y < hslot->y + hslot->height) 
    {
	num_cards = hslot->length;
       
	if ( got_slot == FALSE || num_cards > 0 )
	{  
   		/* if we support exposing more than one card,
		 * find the exact card  */

		gint depth = 1;

		if (hslot->dx > 0)		
			depth += (x - hslot->x) / hslot->dx;
		else if (hslot->dy > 0)
			depth += (y - hslot->y) / hslot->dy;
	    
		/* account for the last card getting much more display area
		 * or no cards */

		if (depth > hslot->exposed)
			
			depth = hslot->exposed;
		*slot = hslot;

		/* card = #cards in slot + card chosen (indexed in # exposed cards) - # exposed cards */

		*cardid = num_cards + depth - hslot->exposed;
		
		/* this is the topmost slot with a card */
		/* take it and run*/
		if ( num_cards > 0 )
			break;

		got_slot = TRUE;
	}
    }
  }
}

void update_slot_length(hslot_type hslot) 
{
  gint delta = g_list_length(hslot->cards) - hslot->length;

  hslot->length += delta;
  hslot->exposed += delta;

  if (0 < hslot->expansion_depth && hslot->expansion_depth < hslot->exposed)
    hslot->exposed = hslot->expansion_depth;

  if ((hslot->dx == 0 && hslot->dy == 0 && hslot->exposed > 1) ||
      (hslot->length > 0 && hslot->exposed < 1))
    hslot->exposed = 1;

  delta = hslot->exposed ? hslot->exposed - 1 : 0;

  hslot->width = get_card_width() + delta * hslot->dx;
  hslot->height = get_card_height() + delta * hslot->dy;
}

GList* get_slot_list() {
  return slot_list;
}

hslot_type get_slot(gint slotid) {
  GList* tempptr;
  
  for (tempptr = slot_list; tempptr; tempptr = tempptr->next)
	 if (((hslot_type) tempptr->data)-> id == slotid)
		return tempptr->data;
  return NULL;
}

void add_cards_to_slot(GList* newcards, hslot_type hslot) {

  hslot->cards = g_list_concat(hslot->cards, newcards);
  update_slot_length(hslot);
}

void delete_slot(hslot_type hslot) {
  GList* temptr;
  
  for (temptr = hslot->cards; temptr; temptr = temptr->next)
    free(temptr->data);
  g_list_free(hslot->cards);
  free(hslot);
}

void delete_surface() {
  GList* temptr;

  for(temptr = slot_list; temptr; temptr = temptr->next) {
    delete_slot(temptr->data);
    temptr->data = NULL;
  }
  g_list_free(slot_list);
  slot_list = NULL;
}
