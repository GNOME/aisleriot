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

#pragma once

#include <gtk/gtk.h>
#include "game.h"

G_BEGIN_DECLS


#define AISLERIOT_TYPE_WINDOW         (aisleriot_window_get_type ())
G_DECLARE_FINAL_TYPE (AisleriotWindow, aisleriot_window, AISLERIOT, WINDOW, GtkApplicationWindow);


GType         aisleriot_window_get_type               (void);
GtkWidget *   aisleriot_window_new                    (GtkApplication  *application);
GtkUIManager *aisleriot_window_get_ui_manager         (AisleriotWindow *window);
GtkAction *   aisleriot_window_get_action             (AisleriotWindow *window,
                                                       const char      *action_name);
void          aisleriot_window_set_game_module        (AisleriotWindow *window,
                                                       const char      *game_module,
                                                       GRand           *rand);
const char *  aisleriot_window_get_game_module        (AisleriotWindow *window);
void          aisleriot_window_new_game               (AisleriotWindow *window);
void          aisleriot_window_change_game            (AisleriotWindow *window);
void          aisleriot_window_show_statistics_dialog (AisleriotWindow *window);
void          aisleriot_window_show_about_dialog      (AisleriotWindow *window);


G_END_DECLS
