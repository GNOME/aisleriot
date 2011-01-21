/*
 *  Copyright © 2007, 2010 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 3, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope conf it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef GAMES_SETTINGS_H
#define GAMES_SETTINGS_H

#include <gio/gio.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

void     games_settings_get_keyval        (GSettings *settings,
                                           const char *key,
                                           guint *keyval,
                                           GdkModifierType *modifiers);
gboolean games_settings_set_keyval        (GSettings *settings,
                                           const char *key,
                                           guint keyval,
                                           GdkModifierType modifiers);
void     games_settings_bind_window_state (const char *path,
                                           GtkWindow *window);

G_END_DECLS

#endif /* !GAMES_GSETTINGS_H */
