/* Aisleriot - slot.c
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

#define SLOT_C
#include "slot.h"
GList *slot_list = NULL;


void slot_pressed(gint x, gint y, gint *slotid, gint *cardid) {
  GList *tempPtr;
  gint i, length;

#ifdef DEBUG
  printf("slot_pressed\n");
  printf ("\tx = %d\ty = %d\n",x,y);
#endif
  for (tempPtr = g_list_last(slot_list); tempPtr; tempPtr = tempPtr->prev) 
	 if ((((hslot_type) tempPtr->data)->x < x) && (((hslot_type) tempPtr->data)->y < y)
		  && ((((hslot_type) tempPtr->data)->width +((hslot_type) tempPtr->data)->x) > x) && 
		  ((((hslot_type) tempPtr->data)->height +((hslot_type) tempPtr->data)->y) > y)) {
		/* we are in a slot... */
		/* which slot? */
		*slotid = ((hslot_type) tempPtr->data)->id;

		/* which card? */
		length = g_list_length(((hslot_type) tempPtr->data)->cards);
		if (((hslot_type) tempPtr->data)->type == NORMAL_SLOT) {
		  *cardid = g_list_length(((hslot_type) tempPtr->data)->cards);
		  return;
		}
		else if ((((hslot_type) tempPtr->data)->type == EXPANDING_SLOT)
			 || ((((hslot_type) tempPtr->data)->type == PARTIALLY_EXPANDING_SLOT)
			     && (length <= (((hslot_type) tempPtr->data)->expansion_depth)))){
		  for (i = 0; i < length; i++) 
			 if ((i* EXPANDED_VERT_OFFSET + ((hslot_type) tempPtr->data)->y) > y) {
				*cardid = i;
				return;
			 }
		  *cardid = length;
		  return;
		}
		else if ((((hslot_type) tempPtr->data)->type == EXPANDING_SLOT_RIGHT)
			 || ((((hslot_type) tempPtr->data)->type == PARTIALLY_EXPANDING_SLOT_RIGHT)
			     && (length <= (((hslot_type) tempPtr->data)->expansion_depth)))){
		  for (i = 0; i < length; i++) 
			 if ((i* EXPANDED_HORIZ_OFFSET + ((hslot_type) tempPtr->data)->x) > x) {
				*cardid = i;
				return;
			 }
		  *cardid = length;
		  return;
		}
		else if (((hslot_type) tempPtr->data)->type == PARTIALLY_EXPANDING_SLOT) {
		  for (i = 0; i < ((hslot_type) tempPtr->data)->expansion_depth; i++) 
		    
		    if ((i* EXPANDED_VERT_OFFSET + ((hslot_type) tempPtr->data)->y) > y) {
		      *cardid = length + i - ((hslot_type) tempPtr->data)->expansion_depth;
		      return;
		    }
		  *cardid = length;
		  return;
		}
		else if (((hslot_type) tempPtr->data)->type == PARTIALLY_EXPANDING_SLOT_RIGHT) {
		  for (i = 0; i < ((hslot_type) tempPtr->data)->expansion_depth; i++) 
		    if ((i* EXPANDED_HORIZ_OFFSET + ((hslot_type) tempPtr->data)->x) > x) {
		      *cardid = length + i - ((hslot_type) tempPtr->data)->expansion_depth;
		      return;
		    }
		  *cardid = length;
		  return;
		  }

	 }
  *slotid = -1;
  *cardid = -1;
  return;
}

GList* get_slot_list() {
  return slot_list;
}

hslot_type get_slot(gint slotid) {
  GList* temp;
#ifdef DEBUG
printf("get_slot\n");
#endif
  
  for (temp = slot_list; temp; temp = temp->next)
	 if (((hslot_type) temp->data)-> id == slotid)
		return temp->data;
  return NULL;
}

void add_slot(hslot_type new_slot) {
#ifdef DEBUG
printf("add_slot\n");
#endif
  if (slot_list)
	 g_list_append(slot_list, new_slot);
  else {
	 slot_list = g_list_alloc();
	 slot_list->data = new_slot;
  }
}

void update_slot_length(gint slotid) {
  hslot_type slot = get_slot(slotid);
#ifdef DEBUG
  printf("update_slot_length\n");
#endif
 
  if (slot) {
	 if (slot->type == EXPANDING_SLOT) {
		slot->width = get_card_width();
		if (slot->cards) { 
		  slot->height = (get_card_height() + (g_list_length(slot->cards) - 1) * EXPANDED_VERT_OFFSET);
		}
		else {
		  slot->height = get_card_height();
		}
	 }
	 if (slot->type == EXPANDING_SLOT_RIGHT) {
		slot->height = get_card_height();
		if (slot->cards) { 
		  slot->width = (get_card_width() + (g_list_length(slot->cards) - 1) * EXPANDED_HORIZ_OFFSET);
		}
		else {
		  slot->width = get_card_width();
		}
	 }
	 if (slot->type == PARTIALLY_EXPANDING_SLOT) {
		slot->width = get_card_width();
		if (slot->cards) { 
		  if (g_list_length(slot->cards) > slot->expansion_depth)
		    slot->height = (get_card_height() + (slot->expansion_depth - 1) * EXPANDED_VERT_OFFSET);
		  else
		    slot->height = (get_card_height() + (g_list_length(slot->cards) - 1) * EXPANDED_VERT_OFFSET);
		}
		else {
		  slot->height = get_card_height();
		}
	 }
	 if (slot->type == PARTIALLY_EXPANDING_SLOT_RIGHT) {
		slot->height = get_card_height();
		if (slot->cards) { 
		  if (g_list_length(slot->cards) > slot->expansion_depth)
		    slot->width = (get_card_width() + (slot->expansion_depth - 1) * EXPANDED_HORIZ_OFFSET);
		  else
		    slot->width = (get_card_width() + (g_list_length(slot->cards) - 1) * EXPANDED_HORIZ_OFFSET);
		}
		else {
		  slot->width = get_card_width();
		}
	 }
	 else if (slot->type == NORMAL_SLOT) {
		slot->width = get_card_width();
		slot->height = get_card_height();
	 }
  }
}

void add_cards_to_slot(GList* newcards, gint slotid) {
  hslot_type slot = get_slot(slotid);
#ifdef DEBUG
printf("add_cards_to_slot\n");
#endif
  if (slot)
	 slot->cards = g_list_concat(slot->cards, newcards);
  update_slot_length(slotid);
}

void delete_slot(hslot_type slot) {
  GList* temptr;
  
  for (temptr = slot->cards; temptr; temptr = temptr->next)
	 free(temptr->data);
  g_list_free(slot->cards);
  free(slot);
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
