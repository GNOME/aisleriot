/* Aisleriot - menu.h
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

#ifndef MENU_H
#define MENU_H

#include <gtk/gtk.h>

#define ELEMENTS(x) (sizeof (x) / sizeof (x [0]))

/* Call backs... */
int file_quit_callback (GtkWidget*, void* );
int game_load_game_callback (GtkWidget*, void* );
int file_new_game_callback (GtkWidget*, void* );
int help_about_callback (GtkWidget*, void* );

GtkMenuFactory *create_menu ();

#endif
