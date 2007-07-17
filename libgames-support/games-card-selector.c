/* games-card-selector.c
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "games-frame.h"
#include "games-files.h"

#include "games-card-common.h"

#include "games-card-selector.h"

enum
{
	CHANGED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

G_DEFINE_TYPE (GamesCardSelector, games_card_selector, GAMES_TYPE_FRAME);

static void
signal_propagator (GtkWidget * widget, GamesCardSelector * selector)
{
  gchar *name;

  name = games_file_list_get_nth (selector->files,
				  gtk_combo_box_get_active (GTK_COMBO_BOX
							    (selector->
							     combobox)));

  g_signal_emit (selector, signals[CHANGED], 0, name);
}

GtkWidget *
games_card_selector_new (gboolean scalable, const gchar * current)
{
  GamesCardSelector *selector;

  selector = g_object_new (GAMES_TYPE_CARD_SELECTOR, NULL);

  games_frame_set_label (GAMES_FRAME (selector), _("Card Style"));

  selector->files = games_file_list_card_themes (scalable);

  selector->combobox = games_file_list_create_widget (selector->files,
						      current,
						      GAMES_FILE_LIST_REPLACE_UNDERSCORES);

  gtk_container_add (GTK_CONTAINER (selector), selector->combobox);

  g_signal_connect (G_OBJECT (selector->combobox), "changed",
		    G_CALLBACK (signal_propagator), selector);

  return GTK_WIDGET (selector);
}
static void
games_card_selector_finalize (GObject *object)
{
  GamesCardSelector *selector = GAMES_CARD_SELECTOR (object);

  g_object_unref (selector->files);

  G_OBJECT_CLASS (games_card_selector_parent_class)->finalize (object);
}

static void
games_card_selector_init (GamesCardSelector * selector)
{
}

static void
games_card_selector_class_init (GamesCardSelectorClass * class)
{
  GObjectClass *oclass = G_OBJECT_CLASS (class);

  oclass->finalize = games_card_selector_finalize;

  signals[CHANGED] =
    g_signal_new ("changed",
		  GAMES_TYPE_CARD_SELECTOR,
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (GamesCardSelectorClass, changed),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__STRING,
		  G_TYPE_NONE, 1, G_TYPE_STRING);
}
