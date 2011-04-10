/*
 * Copyright © 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 * Copyright © 2007, 2010 Christian Persch
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

#ifndef AISLERIOT_BOARD_H
#define AISLERIOT_BOARD_H

#include <gtk/gtk.h>

#include "game.h"
#include "ar-style.h"

G_BEGIN_DECLS

#define AISLERIOT_TYPE_BOARD		(aisleriot_board_get_type ())
#define AISLERIOT_BOARD(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), AISLERIOT_TYPE_BOARD, AisleriotBoard))
#define AISLERIOT_BOARD_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST((k), AISLERIOT_TYPE_BOARD, AisleriotBoardClass))
#define AISLERIOT_IS_BOARD(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), AISLERIOT_TYPE_BOARD))
#define AISLERIOT_IS_BOARD_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), AISLERIOT_TYPE_BOARD))
#define AISLERIOT_BOARD_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), AISLERIOT_TYPE_BOARD, AisleriotBoardClass))

typedef struct _AisleriotBoard		AisleriotBoard;
typedef struct _AisleriotBoardPrivate	AisleriotBoardPrivate;
typedef struct _AisleriotBoardClass	AisleriotBoardClass;

struct _AisleriotBoard {
  GtkDrawingArea parent_instance;

  /*< private >*/
  AisleriotBoardPrivate *priv;
};

struct _AisleriotBoardClass {
  GtkDrawingAreaClass parent_class;

  void (* status_message)   (AisleriotBoard *board,
                             const char *message);

  /* keybinding signals */
  gboolean (* move_cursor)  (AisleriotBoard *board,
                             GtkMovementStep step,
                             int count);
  void (* activate)         (AisleriotBoard *board);
  void (* toggle_selection) (AisleriotBoard *board);
  void (* select_all)       (AisleriotBoard *board);
  void (* deselect_all)     (AisleriotBoard *board);
};

GType aisleriot_board_get_type (void);

GtkWidget *aisleriot_board_new (ArStyle *style,
                                AisleriotGame *game);

void aisleriot_board_abort_move (AisleriotBoard * board);

G_END_DECLS

#endif /* !AISLERIOT_BOARD_H */
