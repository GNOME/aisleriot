/* Aisleriot - press_data.h
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

#ifndef PRESS_DATA_H
#define PRESS_DATA_H
#include <gtk/gtk.h>

typedef struct {
  gint xoffset;
  gint yoffset;
  GdkPixmap *moving_pixmap;
  GdkBitmap *moving_mask;
  gint button_pressed;
  GList* cards;
  gint slot_id;
  gint slot_location;
  gint temporary_partial_hack;
  gboolean moving;
  GdkWindow *moving_cards;
} press_data_type;

void generate_press_data(gint, gint, gint, gint);

#ifndef PRESS_DATA_C
extern press_data_type* press_data; 
#endif

#endif
