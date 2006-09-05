/* 
 * File: dlg_chat.h
 * Author: Jason Short
 * Project: GGZ GTK Games
 * Date: 2/20/2004 (moved from GGZCards)
 * Desc: Create the "Chat" Gtk dialog
 * $Id$
 *
 * Copyright (C) 2004 GGZ Development Team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 */

#include <gtk/gtk.h>
#include <ggzmod.h>

/* Call this function on initialization.  It will register a GGZMod event
 * handler for the CHAT event.  If the game wants its own handler for this
 * event you should register it *after* calling this function, and handle
 * the chat manually. */
void init_chat(GGZMod * ggzmod);

/* Creates a widget containing a chatbox. */
GtkWidget *create_chat_widget(void);

/* Creates or raises the main chat dialog window. */
void create_or_raise_dlg_chat(GtkWindow *window);

void toggle_chat_window(gpointer data, guint action, GtkWidget * w);

void add_chat_text (const gchar *text);
