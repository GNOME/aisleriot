/* Aisleriot - draw.h
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

#ifndef DRAW_H
#define DRAW_H
#include <gtk/gtk.h>

void draw_cards(GdkPixmap* );
void draw_normal_slot(int, int, GdkPixmap* );
void draw_expanding_slot (int, int, GdkPixmap* );
void draw_slot_placements(GdkPixmap* );
void take_snapshot();
void paint_blank_surface();
void refresh_screen();
void stop_show_card();

#endif
