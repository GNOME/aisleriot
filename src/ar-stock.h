/*
 * Copyright © 2005 Richard Hoelscher
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Authors:
 *      Richard Hoelscher <rah@rahga.com>
 */

#pragma once


#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

/* These use stock gtk icons */
#define AR_STOCK_FULLSCREEN       "aisleriot-fullscreen"
#define AR_STOCK_LEAVE_FULLSCREEN "aisleriot-leave-fullscreen"
#define AR_STOCK_NEW_GAME         "aisleriot-game-new"
#define AR_STOCK_START_NEW_GAME   "aisleriot-game-new"
#define AR_STOCK_PAUSE_GAME       "aisleriot-game-pause"
#define AR_STOCK_RESET            "aisleriot-game-reset"
#define AR_STOCK_RESTART_GAME     "aisleriot-game-restart"
#define AR_STOCK_HINT             "aisleriot-game-hint"
#define AR_STOCK_UNDO_MOVE        "aisleriot-move-undo"
#define AR_STOCK_REDO_MOVE        "aisleriot-move-redo"
#define AR_STOCK_CONTENTS         "aisleriot-help-contents"

/* These belong to us */
#define AR_STOCK_DEAL_CARDS       "aisleriot-game-deal"


void   ar_stock_init                           (void);
void   ar_stock_prepare_for_statusbar_tooltips (GtkUIManager *ui_manager,
                                                GtkWidget    *statusbar);
gchar *ar_get_licence                          (const gchar  *game_name);


G_END_DECLS
