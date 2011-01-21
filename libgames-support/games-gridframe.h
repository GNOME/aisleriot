/* games-gridframe.h: Create a container that guarantees that the internal
 *                    allocated space is a fixed multiple of an integer. 
 *
 * Copyright 2004 by Callum McKenzie
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef GAMES_GRID_FRAME_H
#define GAMES_GRID_FRAME_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GAMES_TYPE_GRID_FRAME            (games_grid_frame_get_type ())
#define GAMES_GRID_FRAME(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_GRID_FRAME, GamesGridFrame))
#define GAMES_GRID_FRAME_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_GRID_FRAME, GamesGridFrameClass))
#define GAMES_IS_GRID_FRAME(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_GRID_FRAME))
#define GAMES_IS_GRID_FRAME_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_GRID_FRAME))
#define GAMES_GRID_FRAME_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_GRID_FRAME))

typedef struct GamesGridFramePrivate GamesGridFramePrivate;

typedef struct {
  GtkBin bin;
  /*< private >*/
  GamesGridFramePrivate *priv;
} GamesGridFrame;

typedef struct {
  GtkBinClass parent;
} GamesGridFrameClass;

GType      games_grid_frame_get_type      (void);
GtkWidget *games_grid_frame_new           (gint width,
                                           gint height);
void       games_grid_frame_set           (GamesGridFrame * frame,
                                           gint width,
                                           gint height);
void       games_grid_frame_set_padding   (GamesGridFrame * frame,
                                           gint xpadding,
                                           gint ypadding);
void       games_grid_frame_set_alignment (GamesGridFrame * frame,
                                           gfloat xalign,
                                           gfloat yalign);

G_END_DECLS
#endif /* GAMES_GRID_FRAME_H */
/* EOF */
