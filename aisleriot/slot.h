/* AisleRiot - slot.h
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

#ifndef SLOT_H
#define SLOT_H

#include "glib.h"

/*
 * Data Structures
 */

typedef struct {

  gint id;
  GList* cards;
  gint x;
  gint y;
  gint dx;
  gint dy;
  gint length;
  gint exposed;
  gint expansion_depth;
  gint width;
  gint height;

} slot_type;

typedef slot_type* hslot_type;

extern GList *slot_list;

/*
 * Functions
 */

void slot_pressed (gint, gint, hslot_type *, gint*);
void card_offset (hslot_type, gint, gint*, gint*);
GList* get_slot_list();
void delete_surface();
void delete_slot(hslot_type);
hslot_type get_slot(gint);
void increase_slot_length(hslot_type);
void reduce_slot_length(hslot_type);
void add_cards_to_slot(GList*, hslot_type hslot);
void update_slot_length(hslot_type hslot);

#endif

