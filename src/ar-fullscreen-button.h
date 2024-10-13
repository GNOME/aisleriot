/*
 * Copyright © 2009 Christian Persch <chpe@src.gnome.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

#include <gtk/gtk.h>

G_BEGIN_DECLS


#define AR_TYPE_FULLSCREEN_BUTTON (ar_fullscreen_button_get_type())
G_DECLARE_FINAL_TYPE              (ArFullscreenButton, ar_fullscreen_button, AR, FULLSCREEN_BUTTON, GtkWindow);


GType      ar_fullscreen_button_get_type   (void);
GtkWidget* ar_fullscreen_button_new        (GtkWindow          *parent,
                                            GtkCornerType       corner);
void       ar_fullscreen_button_set_corner (ArFullscreenButton *button,
                                            GtkCornerType       corner);
void       ar_fullscreen_button_set_active (ArFullscreenButton *button,
                                            gboolean            active);


G_END_DECLS
