/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
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

#ifndef __GAMES_FRAME_H__
#define __GAMES_FRAME_H__

#include <gtk/gtkframe.h>

G_BEGIN_DECLS

#define GAMES_TYPE_FRAME            (games_frame_get_type ())
#define GAMES_FRAME(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_FRAME, GamesFrame))
#define GAMES_FRAME_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_FRAME, GamesFrameClass))
#define IS_GAMES_FRAME(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_FRAME))
#define IS_GAMES_FRAME_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_FRAME))
#define GAMES_FRAME_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_FRAME, GamesFrameClass))


  typedef struct _GamesFrame GamesFrame;
  typedef struct _GamesFrameClass GamesFrameClass;

  struct _GamesFrame {
    GtkFrame parent_instance;
  };

  struct _GamesFrameClass {
    GtkFrameClass parent_class;

    /* Padding for future expansion */
    void (*_games_reserved1) (void);
    void (*_games_reserved2) (void);
    void (*_games_reserved3) (void);
    void (*_games_reserved4) (void);
  };


  GType games_frame_get_type (void) G_GNUC_CONST;
  GtkWidget *games_frame_new (const gchar * label);
  void games_frame_set_label (GamesFrame * frame, const gchar * label);

G_END_DECLS

#endif				/* __GAMES_FRAME_H__ */
