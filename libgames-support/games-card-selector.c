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

#include "games-card.h"
#include "games-frame.h"
#include "games-files.h"
#include "games-card-theme.h"

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
  GList *l;
  const char *name;

  l = g_list_nth (selector->files,
                  gtk_combo_box_get_active (GTK_COMBO_BOX (selector->combobox)));
  if (!l)
    return;

  name = l->data;

  g_signal_emit (selector, signals[CHANGED], 0, name);
}

static GtkWidget *
create_combo_box (GList *filelist,
                  const gchar * selection,
                  guint flags)
{
  gint itemno;
  GtkComboBox *widget;
  gchar *visible, *string;
  gboolean found = FALSE;

  widget = GTK_COMBO_BOX (gtk_combo_box_new_text ());

  itemno = 0;
  while (filelist) {
    gchar *s;

    string = (gchar *) filelist->data;
    visible = g_strdup (string);

    /* These are a bit hackish, but we don't yet have a good regexp
     * library in glib. There are probably some ways these could
     * seriously mangle unicode strings. */
    if (flags & GAMES_FILE_LIST_REMOVE_EXTENSION) {
      s = g_strrstr (visible, ".");
      if (s)
        *s = '\0';
    }
    if (flags & GAMES_FILE_LIST_REPLACE_UNDERSCORES) {
      s = visible;
      while (*s) {
        if (*s == '_')
          *s = ' ';
        s++;
      }
    }

    gtk_combo_box_append_text (widget, visible);
    if (selection && (!strcmp (string, selection))) {
      gtk_combo_box_set_active (widget, itemno);
      found = TRUE;
    }

    g_free (visible);

    itemno++;
    filelist = g_list_next (filelist);
  }
  if (!found)
    gtk_combo_box_set_active (widget, 0);

  return GTK_WIDGET (widget);
}

GtkWidget *
games_card_selector_new (gboolean scalable, const gchar * current)
{
  GamesCardSelector *selector;

  selector = g_object_new (GAMES_TYPE_CARD_SELECTOR, NULL);

  games_frame_set_label (GAMES_FRAME (selector), _("Card Style"));

  selector->files = games_card_theme_get_themes ();

  selector->combobox = create_combo_box (selector->files,
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

  g_list_foreach (selector->files, (GFunc) g_free, NULL);
  g_list_free (selector->files);

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
