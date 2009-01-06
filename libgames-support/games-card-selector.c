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

enum {
  COL_INFO,
  COL_NAME,
  N_COLUMNS
};

enum
{
  CHANGED,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

G_DEFINE_TYPE (GamesCardSelector, games_card_selector, GAMES_TYPE_FRAME);

static void
combo_changed_cb (GtkComboBox *combo,
                  GamesCardSelector *selector)
{
  GtkTreeIter iter;
  GamesCardThemeInfo *info;

  if (!gtk_combo_box_get_active_iter (combo, &iter))
    return;

  gtk_tree_model_get (GTK_TREE_MODEL (gtk_combo_box_get_model (combo)), &iter,
                      COL_INFO, &info,
                      -1);
  g_assert (info != NULL);
  
  g_signal_emit (selector, signals[CHANGED], 0, info);
  games_card_theme_info_unref (info);
}

static GtkWidget *
create_combo_box (GamesCardThemes *theme_manager,
                  GamesCardThemeInfo *selected_info)
{
  GtkListStore *store;
  GtkTreeIter iter, selection_iter;
  GtkCellRenderer *renderer;
  gboolean selection_iter_set = FALSE;
  GList *themes, *l;
  GtkWidget *combo;

  store = gtk_list_store_new (N_COLUMNS, GAMES_TYPE_CARD_THEME_INFO, G_TYPE_STRING);

  games_card_themes_request_themes (theme_manager);

  themes = games_card_themes_get_themes (theme_manager);

  for (l = themes; l != NULL; l = l->next) {
    GamesCardThemeInfo *info = l->data;

    gtk_list_store_insert_with_values (store, &iter, -1,
                                       COL_INFO, info,
                                       COL_NAME, games_card_theme_info_get_display_name (info),
                                       -1);
    if (games_card_theme_info_equal (info, selected_info)) {
      selection_iter = iter;
      selection_iter_set = TRUE;
    }
  }

  combo = gtk_combo_box_new_with_model (GTK_TREE_MODEL (store));
  g_object_unref (store);

  renderer = gtk_cell_renderer_text_new ();
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), renderer, TRUE);
  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo), renderer,
                                  "text", COL_NAME,
                                  NULL);

  if (selection_iter_set) {
    gtk_combo_box_set_active_iter (GTK_COMBO_BOX (combo), &selection_iter);
  }

  return combo;
}

GtkWidget *
games_card_selector_new (GamesCardThemes *theme_manager,
                         GamesCardThemeInfo *selected_info)
{
  GamesCardSelector *selector;

  selector = g_object_new (GAMES_TYPE_CARD_SELECTOR, NULL);

  games_frame_set_label (GAMES_FRAME (selector), _("Card Style"));

  selector->combobox = create_combo_box (theme_manager, selected_info);
  g_signal_connect (selector->combobox, "changed",
		    G_CALLBACK (combo_changed_cb), selector);

  gtk_container_add (GTK_CONTAINER (selector), selector->combobox);

  return GTK_WIDGET (selector);
}

static void
games_card_selector_init (GamesCardSelector * selector)
{
}

static void
games_card_selector_class_init (GamesCardSelectorClass *klass)
{
  signals[CHANGED] =
    g_signal_new ("changed",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (GamesCardSelectorClass, changed),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__BOXED,
		  G_TYPE_NONE, 1, GAMES_TYPE_CARD_THEME_INFO);
}
