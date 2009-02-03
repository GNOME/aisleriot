/*
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 * Copyright © 2009 Christian Persch
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
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

#ifndef GAMES_FRAME_H
#define GAMES_FRAME_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GAMES_TYPE_FRAME            (games_frame_get_type ())
#define GAMES_FRAME(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_FRAME, GamesFrame))
#define GAMES_FRAME_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_FRAME, GamesFrameClass))
#define GAMES_IS_FRAME(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_FRAME))
#define GAMES_IS_FRAME_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_FRAME))
#define GAMES_FRAME_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_FRAME, GamesFrameClass))

typedef struct _GamesFrame      GamesFrame;
typedef struct _GamesFrameClass GamesFrameClass;

struct _GamesFrame {
  GtkVBox parent_instance;
  GtkWidget *label;
  GtkWidget *alignment;
};

struct _GamesFrameClass {
  GtkVBoxClass parent_class;
};

GType games_frame_get_type (void);

GtkWidget *games_frame_new (const char *label);

void games_frame_set_label (GamesFrame *frame,
                            const char *label);

G_END_DECLS

#endif /* !GAMES_FRAME_H */
