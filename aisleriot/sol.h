/* Aisleriot - sol.h
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

#ifndef SOL_H
#define SOL_H
#include <gtk/gtk.h>
#include "press_data.h"


/*
 * Constants
 */
#define SURFACE_WIDTH 920
#define SURFACE_HEIGHT 680
#define GAME_EVENTS (GDK_EXPOSURE_MASK              |\
							GDK_BUTTON_PRESS_MASK          |\
							GDK_POINTER_MOTION_MASK        |\
							GDK_BUTTON_RELEASE_MASK)
	  
	  

#ifndef SOL_C
extern GtkWidget* playing_area;
extern GdkPixmap *snapshot;
extern GdkPixmap *blank_surface;
extern GdkPixmap *surface;
extern GdkPixmap *moving_card_pixmap;
gint score;
#endif



#endif
