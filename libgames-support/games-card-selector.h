/* games-card-selector.h
   Copyright © 2004 Callum McKenzie

   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published 
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

/* A widget to select a card theme. */

#ifndef GAMES_CARD_SELECTOR_H
#define GAMES_CARD_SELECTOR_H

#include <gtk/gtk.h>
#include "games-frame.h"
#include "games-files.h"

G_BEGIN_DECLS

typedef struct _GamesCardSelector {
  GamesFrame parent;

  GtkWidget *combobox;
  GamesFileList *files;
} GamesCardSelector;

typedef struct _GamesCardSelectorClass {
  GamesFrameClass parent;

  void (*changed) (GamesCardSelector * selector, gchar * name);
} GamesCardSelectorClass;

#define GAMES_TYPE_CARD_SELECTOR            (games_card_selector_get_type ())
#define GAMES_CARD_SELECTOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_SELECTOR, GamesCardSelector))
#define GAMES_CARD_SELECTOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_SELECTOR, GamesCardSelectorClass))
#define GAMES_IS_CARD_SELECTOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_SELECTOR))
#define GAMES_IS_CARD_SELECTOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_SELECTOR))
#define GAMES_CARD_SELECTOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_SELECTOR))

GType games_card_selector_get_type (void);

GtkWidget *games_card_selector_new (gboolean scalable, const gchar * current);

G_END_DECLS

#endif /* GAMES_CARD_SELECTOR_H */
