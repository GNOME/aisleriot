/* Aisleriot - card.h
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

#ifndef CARD_H
#define CARD_H

#include "gnome.h"

/*
 * Constants
 */

#define UP FALSE
#define DOWN TRUE
#define HORIZ_SPACING 5
#define VERT_SPACING 15
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
#ifndef CARD_C
extern GdkPixmap *default_background_pixmap; 
extern GdkPixmap *slot_pixmap;
extern GdkPixmap *card_back_pixmap;
extern GdkPixmap card_pixmaps[52];
extern GdkBitmap *mask;
#endif
/*
 * Functions
 */
GdkPixmap* get_card_picture(gint, gint);
GdkPixmap* get_slot_pixmap();
GdkPixmap* get_background_pixmap();
GdkPixmap* get_card_back_pixmap();

void load_pixmaps(GtkWidget*);

int get_card_width();
int get_card_height();
int get_horiz_offset();
int get_vert_offset();
int get_vert_start();
int get_horiz_start();

void add_card(GList**, hcard_type);
#endif
