/*
 *  Copyright © 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef AISLERIOT_BOARD_H
#define AISLERIOT_BOARD_H

#include <gtk/gtkdrawingarea.h>
#include "game.h"

G_BEGIN_DECLS

#define AISLERIOT_TYPE_BOARD		(aisleriot_board_get_type ())
#define AISLERIOT_BOARD(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), AISLERIOT_TYPE_BOARD, AisleriotBoard))
#define AISLERIOT_BOARD_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST((k), AISLERIOT_TYPE_BOARD, AisleriotBoardClass))
#define AISLERIOT_IS_BOARD(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), AISLERIOT_TYPE_BOARD))
#define AISLERIOT_IS_BOARD_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), AISLERIOT_TYPE_BOARD))
#define AISLERIOT_BOARD_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), AISLERIOT_TYPE_BOARD, AisleriotBoardClass))

typedef struct _AisleriotBoard		AisleriotBoard;
typedef struct _AisleriotBoardPrivate	AisleriotBoardPrivate;

struct _AisleriotBoard {
  GtkDrawingArea parent_instance;

  /*< private >*/
  AisleriotBoardPrivate *priv;
};

typedef GtkDrawingAreaClass AisleriotBoardClass;

GType aisleriot_board_get_type (void);

GtkWidget *aisleriot_board_new (AisleriotGame * game,
                                gboolean scalable_cards);

gboolean aisleriot_board_set_card_theme (AisleriotBoard * board,
                                         const char *card_theme);

const char *aisleriot_board_get_card_theme (AisleriotBoard * board);

void aisleriot_board_set_click_to_move (AisleriotBoard * board,
                                        gboolean click_to_move);

void aisleriot_board_abort_move (AisleriotBoard * board);

void aisleriot_board_set_pixbuf_drawing (AisleriotBoard * board,
                                         gboolean use_pixbuf_drawing);

G_END_DECLS

#endif /* !AISLERIOT_BOARD_H */
