/* Aisleriot - sol.c
 * Copyright (C) 1998 Felix Bellaby <felix@pooh.u-net.com>
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

#include <glib.h>

/*
 * A simple dialog that indicates progress towards loading an app.
 * Designed for display before entering the gtk_main loop and 
 * hard to convert into an ordinary widget as a result.
 * Currently tailored for use with Aisleriot.
 */

void splash_new ();
void splash_destroy ();

void splash_update (gchar* text, gfloat percent);
