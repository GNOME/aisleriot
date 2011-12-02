/*
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
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

#ifndef UTIL_H
#define UTIL_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

void aisleriot_show_help (GtkWidget *window,
                          const char *game_module);

void ar_atk_util_add_atk_relation (GtkWidget *widget,
                                   GtkWidget *other,
                                   AtkRelationType type);

G_END_DECLS

#endif /* !UTIL_H */
