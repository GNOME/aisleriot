/* Aisleriot - events.h
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

#ifndef EVENTS_H
#define EVENTS_H

#include <gtk/gtk.h>
void drop_moving_cards(gint, gint );
void button_up_not_moved(gint, gint );
gint button_press_event (GtkWidget*, GdkEventButton*, void* );
gint button_release_event (GtkWidget*, GdkEventButton*, void* );
gint configure_event (GtkWidget*, GdkEventConfigure* );
gint motion_notify_event (GtkWidget*, GdkEventMotion* );
gint expose_event (GtkWidget*, GdkEventExpose*, void* );
void end_of_game_test();


#endif
