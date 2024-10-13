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
#include "window.h"

G_BEGIN_DECLS


#define AR_TYPE_GAME_CHOOSER            (ar_game_chooser_get_type())
G_DECLARE_FINAL_TYPE (ArGameChooser, ar_game_chooser, AR, GAME_CHOOSER, GtkDialog);


GType      ar_game_chooser_get_type (void);
GtkWidget *ar_game_chooser_new      (AisleriotWindow *window);


G_END_DECLS
