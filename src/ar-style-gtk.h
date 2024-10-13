/*
 * Copyright © 2009, 2010 Christian Persch <chpe@src.gnome.org>
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

#include "ar-style.h"

G_BEGIN_DECLS

void _ar_style_gtk_class_install_style_properties (GtkWidgetClass *widget_class);
void _ar_style_gtk_attach                         (ArStyle        *style,
                                                   GtkWidget      *widget);
void _ar_style_gtk_detach                         (ArStyle        *style,
                                                   GtkWidget      *widget);


G_END_DECLS
