/*
 * games-stock.h: games stock items and icon registation
 *
 * Copyright (C) 2005 Richard Hoelscher
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

#ifndef __GAMES_STOCK_H__
#define __GAMES_STOCK_H__

#include <glib/gmacros.h>

G_BEGIN_DECLS

/* These uses new icons */
#define GAMES_STOCK_SCORES           "games-scores"
#define GAMES_STOCK_PAUSE_GAME       "games-pause-game"
#define GAMES_STOCK_FULLSCREEN       "games-fullscreen"
#define GAMES_STOCK_LEAVE_FULLSCREEN "games-leave-fullscreen"

/* These use stock gtk icons */
#define GAMES_STOCK_NEW_GAME         "games-new-game"
#define GAMES_STOCK_RESTART_GAME     "games-restart-game"
#define GAMES_STOCK_UNDO_MOVE        "games-undo-move"
#define GAMES_STOCK_REDO_MOVE        "games-redo-move"
#define GAMES_STOCK_HINT             "games-hint"
#define GAMES_STOCK_END_GAME         "games-end-game"
#define GAMES_STOCK_CONTENTS         "games-contents"

void games_stock_init (void);

gchar * games_stock_copy_tooltip_from_stockid(gchar *);

G_END_DECLS

#endif /* __GAMES_STOCK_H__ */
