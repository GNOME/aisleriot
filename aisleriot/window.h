/*
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope window it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef AISLERIOT_WINDOW_H
#define AISLERIOT_WINDOW_H

#include <gtk/gtkwindow.h>

#ifdef HAVE_HILDON
#ifdef HAVE_MAEMO_3
#include <hildon-widgets/hildon-window.h>
#else
#include <hildon/hildon-window.h>
#endif
#endif /* HAVE_HILDON */

#include "game.h"

G_BEGIN_DECLS

#define AISLERIOT_TYPE_WINDOW         (aisleriot_window_get_type ())
#define AISLERIOT_WINDOW(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), AISLERIOT_TYPE_WINDOW, AisleriotWindow))
#define AISLERIOT_WINDOW_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), AISLERIOT_TYPE_WINDOW, AisleriotWindowClass))
#define AISLERIOT_IS_WINDOW(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), AISLERIOT_TYPE_WINDOW))
#define AISLERIOT_IS_WINDOW_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), AISLERIOT_TYPE_WINDOW))
#define AISLERIOT_WINDOW_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), AISLERIOT_TYPE_WINDOW, AisleriotWindowClass))

typedef struct _AisleriotWindow	        AisleriotWindow;
typedef struct _AisleriotWindowPrivate  AisleriotWindowPrivate;

struct _AisleriotWindow {
#ifdef HAVE_HILDON
  HildonWindow parent_instance;
#else
  GtkWindow parent_instance;
#endif /* HAVE_HILDON */

  /*< private >*/
  AisleriotWindowPrivate *priv;
};

#ifdef HAVE_HILDON
typedef HildonWindowClass AisleriotWindowClass;
#else
typedef GtkWindowClass AisleriotWindowClass;
#endif /* HAVE_HILDON */

GType aisleriot_window_get_type (void);

GtkWidget *aisleriot_window_new (void);

void aisleriot_window_set_freecell_mode (AisleriotWindow * window);

void aisleriot_window_set_game (AisleriotWindow * window,
                                const char *game_file, guint seed);

AisleriotGame *aisleriot_window_get_game (AisleriotWindow * window);

G_END_DECLS

#endif /* !AISLERIOT_WINDOW_H */
