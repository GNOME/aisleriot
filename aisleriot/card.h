/* AisleRiot - card.h
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

#ifndef CARD_H
#define CARD_H

#include <gnome.h>
#include "../freecell/gdk-card-image/gdk-card-image.h"

/*
 * Constants
 */

#define UP FALSE
#define DOWN TRUE

/*
 * Data Structures
 */

typedef struct {
  gint suit;
  gint value;
  gboolean direction;
} card_type;
typedef card_type* hcard_type;


/*
 * Variables
 */
extern GdkBitmap *mask;

/*
 * Functions
 */
GdkPixmap* get_card_picture (gint, gint);
GdkPixmap* get_slot_pixmap ();
GdkPixmap* get_background_pixmap ();
GdkPixmap* get_card_back_pixmap ();

void load_pixmaps (GtkWidget*, GdkCardDeckOptions);
void free_pixmaps (void);

int get_card_width ();
int get_card_height ();
int get_horiz_offset ();
int get_vert_offset ();
int get_vert_start ();
int get_horiz_start ();

void add_card (GList**, hcard_type);
#endif
