/* Aisleriot - sol.h
 * Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
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
#include <gconf/gconf-client.h>
#include <gtk/gtk.h>
#include "press_data.h"
#include "gdk-card-image.h"

/*
 * Constants
 */
#define GAMESDIR "sol-games/"
#define GAME_EVENTS (GDK_EXPOSURE_MASK        |\
		     GDK_BUTTON_PRESS_MASK    |\
		     GDK_POINTER_MOTION_MASK  |\
		     GDK_BUTTON_RELEASE_MASK)

/* GConf keys. */
#define WIDTH_GCONF_KEY "/apps/aisleriot/width"
#define HEIGHT_GCONF_KEY "/apps/aisleriot/height"

/*
 * Global variables
 */

extern GtkWidget*       app;
extern GtkWidget*       playing_area;
extern GtkWidget*       option_dialog;
extern GdkGC*           draw_gc;
extern GdkPixmap*       surface;
extern GdkPixmap*       moving_card_pixmap;
extern GtkObject*       card_deck;
extern GdkCardDeckOptions deck_options;

extern guint            score;
extern guint            game_time;
extern guint            timeout;
extern guint            seed;
extern guint            n_games;
extern struct dirent**  game_dents;
extern gchar*           game_file;
extern gchar*           game_name;
extern gboolean         game_in_progress;
extern gboolean         game_over;
extern gboolean         game_won;

extern guint            x_spacing;
extern guint            y_spacing;
extern double            x_expanded_offset;
extern double            y_expanded_offset;

extern GConfClient * gconf_client;

gchar* game_file_to_name(const gchar* file);
void new_game(gchar* file, guint *seed);
void quit_app (GtkMenuItem*);
void set_score( void );
void timer_start( void );
void timer_stop( void );
void timer_reset (void);
guint timer_get (void);
void make_title( void );
void eval_installed_file(gchar *file);

#endif
