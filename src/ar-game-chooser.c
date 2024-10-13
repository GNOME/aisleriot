/*  
 * Copyright © 2009 Christian Persch <chpe@src.gnome.org>
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

#include <config.h>

#include "ar-game-chooser.h"

#include <string.h>

#include <glib/gi18n.h>

#include "ar-debug.h"
#include "ar-runtime.h"
#include "ar-string-utils.h"
#include "conf.h"


#define SELECT_GAME_DIALOG_MIN_WIDTH (300)
#define SELECT_GAME_DIALOG_MIN_HEIGHT (256)
#define SELECTED_PATH_DATA_KEY "selected-path"
#define MAX_RECENT 5


enum {
  PROP_0,
  PROP_WINDOW
};

enum {
  COL_NAME,
  COL_GAME_MODULE,
  COL_RECENT_RANK
};


struct _ArGameChooser
{
  GtkDialog parent;

  AisleriotWindow *window;

  GtkListStore *store;
  GtkTreeSelection *selection;
};


G_DEFINE_TYPE (ArGameChooser, ar_game_chooser, GTK_TYPE_DIALOG);


/* private functions */

static void
row_activated_cb (GtkWidget *widget,
                  GtkTreePath *path,
                  GtkTreeViewColumn *column,
                  ArGameChooser *chooser)
{
  /* Handle a double click by faking a click on the OK button. */
  gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);
}

static void
response_cb (GtkWidget *dialog,
             int response,
             ArGameChooser *self)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  if (response == GTK_RESPONSE_OK &&
      gtk_tree_selection_get_selected (self->selection, &model, &iter)) {
    char *game_module = NULL;

    gtk_tree_model_get (model, &iter,
                        COL_GAME_MODULE, &game_module,
                        -1);
    g_assert (game_module != NULL);

    aisleriot_window_set_game_module (self->window, game_module, NULL);

    g_free (game_module);
  }

  gtk_widget_destroy (dialog);
}

/* GObjectClass impl */

static void
ar_game_chooser_init (ArGameChooser *chooser)
{
  /* Pass */
}

static void
store_add_module (GtkListStore *store,
                  const char *game_module,
                  int recent_rank)
{
  char *game_name;
  GtkTreeIter iter;

  game_name = ar_filename_to_display_name (game_module);
  gtk_list_store_insert_with_values (store, &iter,
                                     -1,
                                     COL_NAME, game_name,
                                     COL_GAME_MODULE, game_module,
                                     COL_RECENT_RANK, recent_rank,
                                     -1);
  g_free (game_name);
}

static void
store_add_separator (GtkListStore *store)
{
  GtkTreeIter iter;

  gtk_list_store_insert_with_values (store, &iter,
                                     -1,
                                     COL_NAME, NULL,
                                     COL_GAME_MODULE, NULL,
                                     COL_RECENT_RANK, 0,
                                     -1);
}

static gboolean
row_separator_func (GtkTreeModel *model,
                    GtkTreeIter *iter,
                    gpointer data)
{
  gboolean ret;
  char *game_module;

  gtk_tree_model_get (model, iter,
                      COL_GAME_MODULE, &game_module,
                      -1);
  ret = (game_module == NULL);
  g_free (game_module);

  return ret;
}

static char **
get_unique_recent_modules (const char *current_module)
{
  char **recent_games, **new_recent;
  gsize i;
  gsize n_recent = 0;
  gsize n_new_recent = 0;

  recent_games = ar_conf_get_string_list (NULL, aisleriot_conf_get_key (CONF_RECENT_GAMES), &n_recent, NULL);

  if (recent_games == NULL) {
    new_recent = g_new (char *, 2);
    new_recent[0] = g_strdup (current_module);
    new_recent[1] = NULL;
    n_new_recent = 1;
  } else {
    new_recent = g_new (char *, MIN (n_recent + 1, MAX_RECENT) + 1);
    n_new_recent = 0;

    new_recent[n_new_recent++] = g_strdup (current_module);

    for (i = 0; i < n_recent && n_new_recent < MAX_RECENT; ++i) {
      const char *module = recent_games[i];
      gboolean found = FALSE;
      gsize j;

      for (j = 0; j < n_new_recent; j++) {
        const char *existing = new_recent[j];

        if (g_ascii_strcasecmp (existing, module) == 0) {
          found = TRUE;
          break;
        }
      }

      if (!found)
        new_recent[n_new_recent++] = g_strdup (recent_games[i]);
    }

    /* NULL termination */
    new_recent[n_new_recent] = NULL;

    g_strfreev (recent_games);
  }

  return new_recent;
}

static void
add_recent_items (ArGameChooser *self)
{
  const char *current_game_module;
  char **games;
  int i;

  current_game_module = aisleriot_window_get_game_module (self->window);
  games = get_unique_recent_modules (current_game_module);

  for (i = 0; games[i] != NULL; ++i) {
    store_add_module (self->store, games[i], MAX_RECENT - i);
  }

  g_strfreev (games);

  store_add_separator (self->store);
}

static int
sort_func (GtkTreeModel *model,
           GtkTreeIter *iter_a,
           GtkTreeIter *iter_b,
           gpointer data)
{
  int ret;
  int recent_a, recent_b;
  char *name_a, *name_b;

  gtk_tree_model_get (model, iter_a,
                      COL_NAME, &name_a,
                      COL_RECENT_RANK, &recent_a,
                      -1);
  gtk_tree_model_get (model, iter_b,
                      COL_NAME, &name_b,
                      COL_RECENT_RANK, &recent_b,
                      -1);
  if (recent_a < recent_b) {
    ret = 1;
  } else if (recent_a > recent_b) {
    ret = -1;
  } else {
    ret = g_utf8_collate (name_a, name_b);
  }

  g_free (name_a);
  g_free (name_b);

  return ret;
}

static GObject *
ar_game_chooser_constructor (GType type,
                             guint n_construct_properties,
                             GObjectConstructParam *construct_params)
{
  GObject *object;
  ArGameChooser *chooser;
  GtkWindow *window;
  GtkListStore *list;
  GtkWidget *list_view;
  GtkTreeSelection *selection;
  GtkWidget *scrolled_window;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GtkWidget *hbox;
  GtkTreePath *path;
  char **games;
  int i;
  GtkWidget *content_area, *action_area;
  GtkDialog *dialog;

  object = G_OBJECT_CLASS (ar_game_chooser_parent_class)->constructor
            (type, n_construct_properties, construct_params);

  window = GTK_WINDOW (object);
  chooser = AR_GAME_CHOOSER (object);

  g_assert (chooser->window != NULL);

  chooser->store = list = gtk_list_store_new (3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT);

  add_recent_items (chooser);

  games = ar_get_game_modules ();
  if (games != NULL) {
    for (i = 0; games[i]; ++i) {
      const char *game_module = games[i];

      store_add_module (chooser->store, game_module, -1);
    }
  }

  g_strfreev (games);

  /* Now construct the window contents */
  gtk_window_set_title (window, _("Select Game"));
  gtk_window_set_modal (window, TRUE);

  dialog = GTK_DIALOG (object);

  g_signal_connect (dialog, "response",
                    G_CALLBACK (response_cb), chooser);

  gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
  content_area = gtk_dialog_get_content_area (dialog);
  gtk_box_set_spacing (GTK_BOX (content_area), 2);

  gtk_dialog_add_buttons (dialog,
                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                          _("_Select"), GTK_RESPONSE_OK,
                          NULL);
  gtk_dialog_set_alternative_button_order (dialog,
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           -1);
  gtk_dialog_set_default_response (dialog, GTK_RESPONSE_OK);

  list_view = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list));
  g_object_unref (list);

  g_signal_connect (list_view, "row-activated",
                    G_CALLBACK (row_activated_cb), chooser);

  gtk_tree_view_set_row_separator_func (GTK_TREE_VIEW (list_view),
                                        (GtkTreeViewRowSeparatorFunc)row_separator_func,
                                        NULL, NULL);

  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (list),
                                        0, GTK_SORT_ASCENDING);
  gtk_tree_sortable_set_sort_func (GTK_TREE_SORTABLE (list),
                                   0, (GtkTreeIterCompareFunc)sort_func,
                                   NULL, NULL);

  hbox = gtk_hbox_new (FALSE, 12);
  gtk_container_set_border_width (GTK_CONTAINER (hbox), 5);
  gtk_box_pack_start (GTK_BOX (content_area), hbox, TRUE, TRUE, 0);

  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (list_view), FALSE);

  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes (NULL,
                                                     renderer,
                                                     "text", COL_NAME, NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (list_view), column);

  chooser->selection = selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (list_view));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolled_window),
                                       GTK_SHADOW_IN);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                  GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  gtk_container_add (GTK_CONTAINER (scrolled_window), list_view);

  gtk_box_pack_end (GTK_BOX (hbox), scrolled_window, TRUE, TRUE, 0);

  /* Set initial focus to the list view. Otherwise, if you press Up/Down key,
   * the selection jumps to the first entry in the list as the view gains
   * focus.
   */
  gtk_widget_grab_focus (list_view);

  gtk_widget_show_all (hbox);

  gtk_widget_set_size_request (scrolled_window,
                               SELECT_GAME_DIALOG_MIN_WIDTH,
                               SELECT_GAME_DIALOG_MIN_HEIGHT);

  /* Select the row corresponding to the currently loaded game,
   * and scroll to it.
   */
  path = gtk_tree_path_new_first ();
  gtk_tree_selection_select_path (selection, path);

  /* Scroll view to the current item */
  gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (list_view), path, NULL,
                                TRUE,
                                0.5, 0.0);
  gtk_tree_path_free (path);

  /* Fixup dialogue padding, #735242 */
  action_area = gtk_dialog_get_action_area (GTK_DIALOG (dialog));
  gtk_widget_set_margin_left   (action_area, 5);
  gtk_widget_set_margin_right  (action_area, 5);
  gtk_widget_set_margin_top    (action_area, 5);
  gtk_widget_set_margin_bottom (action_area, 5);

  return object;
}

static void
ar_game_chooser_set_property (GObject      *object,
                              guint         property_id,
                              const GValue *value,
                              GParamSpec   *pspec)
{
  ArGameChooser *chooser = AR_GAME_CHOOSER (object);

  switch (property_id) {
    case PROP_WINDOW:
      chooser->window = g_value_get_object (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_game_chooser_class_init (ArGameChooserClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructor = ar_game_chooser_constructor;
  object_class->set_property = ar_game_chooser_set_property;

  /**
   * ArGameChooser:window:
   *
   * The parent #AisleriotWindow
   */
  g_object_class_install_property
    (object_class,
     PROP_WINDOW,
     g_param_spec_object ("window", NULL, NULL,
                          AISLERIOT_TYPE_WINDOW,
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));
}

/* public API */

/**
 * ar_game_chooser_new:
 * @window: the parent #AisleriotWindow
 *
 * Return value: a new #ArGameChooser
 */
GtkWidget *
ar_game_chooser_new (AisleriotWindow *window)
{
  return g_object_new (AR_TYPE_GAME_CHOOSER,
                       "type", GTK_WINDOW_TOPLEVEL,
                       "transient-for", window,
                       "destroy-with-parent", TRUE,
                       "window", window,
                       NULL);
}
