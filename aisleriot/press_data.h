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
#include "slot.h"

typedef struct {
  gint xoffset;
  gint yoffset;
  GdkPixmap *moving_pixmap;
  GdkBitmap *moving_mask;
  GList* cards;
  hslot_type hslot;
  gint cardid;
  gint status;
  GdkWindow *moving_cards;
} press_data_type;

#define STATUS_NONE 0
#define STATUS_IS_DRAG 1
#define STATUS_MAYBE_DRAG 2
#define STATUS_NOT_DRAG 3
#define STATUS_SHOW 4

void generate_press_data();

#endif
