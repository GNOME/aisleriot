/* Aisleriot - slot.h
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

#ifndef SLOT_H
#define SLOT_H

#include "glib.h"

/*
 * Data Structures
 */

typedef struct {
  gint x;
  gint y;
  gint width;
  gint height;
  gint type; 
  GList* cards;
  gint id;
  gint expansion_depth;
} slot_type;

typedef slot_type* hslot_type;

/*
 * Variables
 */
#define EXPANDED_VERT_OFFSET 20
#define EXPANDED_HORIZ_OFFSET 20

#define NORMAL_SLOT 0
#define EXPANDING_SLOT 1
#define EXPANDING_SLOT_RIGHT 2
#define PARTIALLY_EXPANDING_SLOT 3
#define PARTIALLY_EXPANDING_SLOT_RIGHT 4
#define SCHEME_DEFINED_SLOT 5

extern GList *slot_list;

/*
 * Functions
 */

void slot_pressed(gint, gint, gint*, gint*);
GList* get_slot_list();
void delete_surface();
void add_slot(hslot_type);
void delete_slot(hslot_type);
hslot_type get_slot(gint);
void update_slot_length(gint);
void add_cards_to_slot(GList*, gint);

#endif

