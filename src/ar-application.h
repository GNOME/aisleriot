/*
 * Copyright © 2013 William Jon McCann
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

#define AR_TYPE_APPLICATION (ar_application_get_type ())
G_DECLARE_FINAL_TYPE        (ArApplication, ar_application, AR, APPLICATION, GtkApplication);

GType           ar_application_get_type (void);
GtkApplication *ar_application_new      (const char *variation,
                                         gboolean    freecell);

G_END_DECLS
