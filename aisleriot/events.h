/* AisleRiot - events.h
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

#ifndef EVENTS_H
#define EVENTS_H

#include <gtk/gtk.h>
void drop_moving_cards(gint, gint );
void button_up_not_moved(gint, gint );
gint button_press_event (GtkWidget*, GdkEventButton*, void* );
gint button_release_event (GtkWidget*, GdkEventButton*, void* );
gint configure_event (GtkWidget*, GdkEventConfigure* );
gint motion_notify_event (GtkWidget*, GdkEventMotion* );
void end_of_game_test(void);
int waiting_for_mouse_up(void);

enum {
  STATUS_NONE,
  STATUS_MAYBE_DRAG,
  STATUS_NOT_DRAG,
  STATUS_IS_DRAG,
  STATUS_CLICK,
  STATUS_SHOW
};

#endif
