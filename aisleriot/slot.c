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

#include "slot.h"
#include <stdio.h>

GList *slot_list = NULL;

void slot_pressed(gint x, gint y, gint *slotid, gint *cardid) {
  GList *tempptr;

  *slotid = -1;
  *cardid = -1;

  for (tempptr = slot_list; tempptr; tempptr = tempptr->next) {

    hslot_type hslot = (hslot_type) tempptr->data;
    gint depth, length;

    if (hslot->x < x && x < hslot->x + hslot->width && 
	hslot->y < y && y < hslot->y + hslot->height) {

      *slotid = hslot->id;
      
      length = g_list_length(hslot->cards);

      switch (hslot->type) {

      case NORMAL_SLOT:
	depth = length;
	break;

      case EXPANDING_SLOT:
      case PARTIALLY_EXPANDING_SLOT:
	depth = (y - hslot->y) / EXPANDED_VERT_OFFSET + 1;
	break;

      case EXPANDING_SLOT_RIGHT:
      case PARTIALLY_EXPANDING_SLOT_RIGHT:
	depth = (x - hslot->x) / EXPANDED_HORIZ_OFFSET + 1;
	break;
      }

      if ((hslot->type == PARTIALLY_EXPANDING_SLOT ||
	   hslot->type == PARTIALLY_EXPANDING_SLOT_RIGHT) &&
	  length > hslot->expansion_depth) 
	depth += length - hslot->expansion_depth;

      if (depth > length) 
	depth = length;

      *cardid = depth;
    }
  }
  return;
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

void add_slot(hslot_type new_slot) {
  if (slot_list)
	 g_list_append(slot_list, new_slot);
  else {
	 slot_list = g_list_alloc();
	 slot_list->data = new_slot;
  }
}

void update_slot_length(gint slotid) {
  hslot_type slot = get_slot(slotid);
 
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
