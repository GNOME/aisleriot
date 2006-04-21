/* AisleRiot - slot.h
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

#ifndef SLOT_H
#define SLOT_H

#include <glib.h>

/*
 * Data Structures
 */

struct _slot_struct {
  gint id;
  GList* cards;
  /* The location in slot co-ordinates. Filled in by the game code. */
  double x;
  double y;
  double dx;
  double dy;
  /* there is the real dy, and the one we calculate so that
   * the slot fits in the screen. we compress too tall slots
   * see bug#171417 */
  double compressed_dy;
  
  /* The location in pixel units. Filled in by the scaling code. */
  gint pixelx;
  gint pixely;
  gint pixeldx;
  gint pixeldy;
  gint length;
  gint exposed;
  gint expansion_depth;
  gint width;
  gint height;
};

typedef struct _slot_struct* hslot_type;

/*
 * Functions
 */

void slot_pressed (gint, gint, hslot_type *, gint*);
void card_offset (hslot_type, gint, gint*, gint*);
GList* get_slot_list(void);
void delete_surface(void);
hslot_type get_slot(gint);
void add_cards_to_slot(GList*, hslot_type hslot);
void update_slot_length(hslot_type hslot);

void add_slot(gint id, GList *cards, double x, double y, 
	      gboolean expanded_down, gboolean expanded_right, 
	      gint expansion_depth);

#endif

