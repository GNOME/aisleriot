/*
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

#ifndef GAMES_STRING_UTILS_H
#define GAMES_STRING_UTILS_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

char *ar_filename_to_display_name (const char *filename);

char *ar_filename_to_game_module (const char *game_file);

G_END_DECLS

#endif /* !GAMES_STRING_UTILS_H */
