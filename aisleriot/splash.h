/* AisleRiot - splash.h
 * Copyright (C) 1998 Felix Bellaby <felix@pooh.u-net.com>
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

#include <glib.h>

/*
 * A simple dialog that indicates progress towards loading an app.
 * Designed for display before entering the gtk_main loop and 
 * hard to convert into an ordinary widget as a result.
 * Currently tailored for use with Aisleriot.
 */

void splash_new ( void );
void splash_destroy ( void );

void splash_update (gchar* text, gfloat percent);
