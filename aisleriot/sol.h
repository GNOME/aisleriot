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
#define GAMESDIR "sol-games/"
#define GAME_EVENTS (GDK_EXPOSURE_MASK        |\
		     GDK_BUTTON_PRESS_MASK    |\
		     GDK_POINTER_MOTION_MASK  |\
		     GDK_BUTTON_RELEASE_MASK)
/*
 * Global variables
 */
extern GtkWidget        *app;
extern GtkWidget        *playing_area;
extern GdkGC            *draw_gc;
extern GdkPixmap        *surface;
extern GdkPixmap        *moving_card_pixmap;
extern guint            score;
extern guint            game_time;
extern guint            timeout;
extern guint            seed;
extern guint            n_games;
extern struct dirent    **game_dents;
extern gchar            *game_file;
extern gchar            *game_name;
extern press_data_type* press_data; 

gchar* game_file_to_name(const gchar* file);
void set_score();
void timer_start();
void timer_stop();
void make_title();
void eval_installed_file(gchar *file);

#endif
