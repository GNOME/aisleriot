/* AisleRiot - press_data.h
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

void generate_press_data ( void );

#endif
