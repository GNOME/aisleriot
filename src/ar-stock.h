/*
 * Copyright © 2005 Richard Hoelscher
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * Authors:
 *      Richard Hoelscher <rah@rahga.com>
 */

#ifndef __AR_STOCK_H__
#define __AR_STOCK_H__

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

/* These use stock gtk icons */
#define AR_STOCK_SCORES           "games-scores"
#define AR_STOCK_PAUSE_GAME       "games-pause-game"
#define AR_STOCK_RESUME_GAME      "games-resume-game"

#define AR_STOCK_FULLSCREEN       "games-fullscreen"
#define AR_STOCK_LEAVE_FULLSCREEN "games-leave-fullscreen"
#define AR_STOCK_NEW_GAME         "games-new-game"
#define AR_STOCK_START_NEW_GAME   "games-start-new-game"
#define AR_STOCK_NETWORK_GAME     "games-network-game"
#define AR_STOCK_NETWORK_LEAVE    "games-network-leave"
#define AR_STOCK_PLAYER_LIST      "games-player-list"
#define AR_STOCK_RESTART_GAME     "games-restart-game"
#define AR_STOCK_UNDO_MOVE        "games-undo-move"
#define AR_STOCK_REDO_MOVE        "games-redo-move"
#define AR_STOCK_HINT             "games-hint"
#define AR_STOCK_END_GAME         "games-end-game"
#define AR_STOCK_CONTENTS         "games-contents"
#define AR_STOCK_RESET            "games-reset"

/* These belong to us */
#define AR_STOCK_TELEPORT         "games-teleport"
#define AR_STOCK_RTELEPORT        "games-teleport-random"
#define AR_STOCK_DEAL_CARDS       "games-cards-deal"

void   ar_stock_init (void);
void   ar_stock_prepare_for_statusbar_tooltips (GtkUIManager * ui_manager,
                                                   GtkWidget * statusbar);
gchar *ar_get_licence (const gchar * game_name);

G_END_DECLS

#endif /* __AR_STOCK_H__ */
