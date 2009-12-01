/*  
 * Copyright © 2009 Christian Persch <chpe@gnome.org>
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

#include <libgames-support/games-debug.h>
#include <libgames-support/games-runtime.h>
#include <libgames-support/games-string-utils.h>

struct _ArGameChooser
{
  GtkDialog parent;

  /*< private >*/
  ArGameChooserPrivate *priv;
};

struct _ArGameChooserClass
{
  GtkDialogClass parent_class;
};

struct _ArGameChooserPrivate {
  AisleriotWindow *window;

  GtkListStore *store;
  GtkTreeSelection *selection;
};

enum {
  PROP_0,
  PROP_WINDOW
};

enum {
  COL_NAME,
  COL_GAME_FILE
};

#define SELECT_GAME_DIALOG_MIN_HEIGHT (256)
#define SELECTED_PATH_DATA_KEY "selected-path"

/* private functions */
    
static void
row_activated_cb (GtkWidget *widget,
                  GtkTreePath *path,
                  GtkTreeViewColumn *column,
                  ArGameChooser *chooser)
{
  g_assert (path != NULL);

#ifdef HAVE_MAEMO_5
  /* On maemo5 with a treeview in a pannable area, the selection
   * handling is special. There _never_ is any row selected!
   * Instead, we get a row-activated signal (which normally is only
   * emitted when double-clicking a row) when the user selects a row.
   *
   * Since we can't use the selection, we need to store the activated
   * row somewhere else.
   */
  g_object_set_data_full (G_OBJECT (chooser), SELECTED_PATH_DATA_KEY,
                          gtk_tree_path_copy (path),
                          (GDestroyNotify) gtk_tree_path_free);
#endif /* HAVE_MAEMO_5 */

  /* Handle a double click by faking a click on the OK button. */
  gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);
}

static gboolean
delete_cb (GtkDialog *dialog,
           ArGameChooser *chooser)
{
  g_print ("delete!\n");
  gtk_dialog_response (dialog, GTK_RESPONSE_DELETE_EVENT);
  return TRUE;
}

static void
response_cb (GtkWidget *dialog,
             int response,
             ArGameChooser *chooser)
{
  ArGameChooserPrivate *priv = chooser->priv;
  GtkTreeModel *model;
  GtkTreeIter iter;
  char *game_file = NULL;
#ifdef HAVE_MAEMO_5
  GtkTreePath *path;
#endif

  if (response != GTK_RESPONSE_OK)
    goto done;

#ifdef HAVE_MAEMO_5
  /* See the comment in select_game_row_activated_cb() above. */
  if ((path = g_object_get_data (G_OBJECT (dialog), SELECTED_PATH_DATA_KEY)) != NULL &&
      gtk_tree_model_get_iter (GTK_TREE_MODEL (priv->store), &iter, path)) {
    model = GTK_TREE_MODEL (priv->store);
#else
  if (gtk_tree_selection_get_selected (priv->selection, &model, &iter)) {
#endif
    gtk_tree_model_get (model, &iter,
                        COL_GAME_FILE, &game_file,
                        -1);
    g_assert (game_file != NULL);

    aisleriot_window_set_game (priv->window, game_file, 0);

    /* We'll store the new game in conf when it's been loaded successfully */

    g_free (game_file);
  }

done:
  gtk_widget_destroy (dialog);
}

/* GType impl */

G_DEFINE_TYPE (ArGameChooser, ar_game_chooser, GTK_TYPE_DIALOG)

/* GObjectClass impl */

static void
ar_game_chooser_init (ArGameChooser *chooser)
{
  chooser->priv = G_TYPE_INSTANCE_GET_PRIVATE (chooser, AR_TYPE_GAME_CHOOSER, ArGameChooserPrivate);
}

static GObject *
ar_game_chooser_constructor (GType type,
                             guint n_construct_properties,
                             GObjectConstructParam *construct_params)
{
  GObject *object;
  ArGameChooser *chooser;
  ArGameChooserPrivate *priv;
  GtkWidget *widget;
  GtkWindow *window;
  GtkDialog *dialog;
  GtkListStore *list;
  GtkWidget *list_view;
  GtkTreeSelection *selection;
  GtkWidget *scrolled_window;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GtkWidget *hbox, *content_area;
  GtkTreeIter current_iter;
  gboolean current_iter_set = FALSE;
  const char *current_game_file;
  const char *games_dir;
  GDir *dir;

  object = G_OBJECT_CLASS (ar_game_chooser_parent_class)->constructor
            (type, n_construct_properties, construct_params);

  widget = GTK_WIDGET (object);
  window = GTK_WINDOW (object);
  dialog = GTK_DIALOG (object);
  chooser = AR_GAME_CHOOSER (object);
  priv = chooser->priv;

  g_assert (priv->window != NULL);

  gtk_dialog_set_has_separator (dialog, FALSE);
  gtk_dialog_add_buttons (dialog,
                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                          _("_Select"), GTK_RESPONSE_OK,
                          NULL);
  gtk_dialog_set_alternative_button_order (dialog,
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           -1);
  gtk_dialog_set_default_response (dialog, GTK_RESPONSE_OK);

  priv->store = list = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);

#ifdef HAVE_MAEMO_5
  list_view = hildon_gtk_tree_view_new_with_model (HILDON_UI_MODE_NORMAL, GTK_TREE_MODEL (list));
#else
  list_view = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list));
#endif
  g_object_unref (list);

  current_game_file = aisleriot_game_get_game_file (aisleriot_window_get_game (priv->window));

  games_dir = games_runtime_get_directory (GAMES_RUNTIME_GAME_GAMES_DIRECTORY);

  dir = g_dir_open (games_dir, 0, NULL);
  if (dir != NULL) {
    const char *game_file;

    while ((game_file = g_dir_read_name (dir)) != NULL) {
      char *game_name;
      GtkTreeIter iter;

      if (!g_str_has_suffix (game_file, ".scm") ||
          strcmp (game_file, "sol.scm") == 0)
        continue;

      game_name = games_filename_to_display_name (game_file);

      gtk_list_store_insert_with_values (GTK_LIST_STORE (list), &iter,
                                         -1,
                                         COL_NAME, game_name,
                                         COL_GAME_FILE, game_file,
                                         -1);

      if (current_game_file &&
          strcmp (current_game_file, game_file) == 0) {
        current_iter = iter;
        current_iter_set = TRUE;
      }

      g_free (game_name);
    }

    g_dir_close (dir);
  }

  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (list),
                                        0, GTK_SORT_ASCENDING);

  gtk_window_set_title (window, _("Select Game"));
  gtk_window_set_modal (window, TRUE);

  gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
  content_area = gtk_dialog_get_content_area (dialog);
  gtk_box_set_spacing (GTK_BOX (content_area), 2);

  hbox = gtk_hbox_new (FALSE, 12);
  gtk_container_set_border_width (GTK_CONTAINER (hbox), 5);
  gtk_box_pack_start (GTK_BOX (content_area), hbox, TRUE, TRUE, 0);
  g_signal_connect (list_view, "row-activated",
                    G_CALLBACK (row_activated_cb), dialog);

  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (list_view), FALSE);

  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes (NULL,
                                                     renderer,
                                                     "text", COL_NAME, NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (list_view), column);

  priv->selection = selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (list_view));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

#ifdef HAVE_MAEMO_5
  scrolled_window = hildon_pannable_area_new ();
#else
  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolled_window),
                                       GTK_SHADOW_IN);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                  GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
#endif /* HAVE_MAEMO_5 */

  gtk_container_add (GTK_CONTAINER (scrolled_window), list_view);

  gtk_box_pack_end (GTK_BOX (hbox), scrolled_window, TRUE, TRUE, 0);

  /* Set initial focus to the list view. Otherwise, if you press Up/Down key,
   * the selection jumps to the first entry in the list as the view gains
   * focus.
   */
  gtk_widget_grab_focus (list_view);

  gtk_widget_show_all (hbox);

  g_signal_connect (dialog, "response",
                    G_CALLBACK (response_cb), chooser);
  g_signal_connect (dialog, "delete-event",
                    G_CALLBACK (delete_cb), chooser);

  gtk_widget_set_size_request (scrolled_window, -1, SELECT_GAME_DIALOG_MIN_HEIGHT);

  /* Select the row corresponding to the currently loaded game,
   * and scroll to it.
   */
  if (current_iter_set) {
    GtkTreePath *path;

    gtk_tree_selection_select_iter (selection, &current_iter);

    /* Scroll view to the current item */
    path = gtk_tree_model_get_path (GTK_TREE_MODEL (list), &current_iter);
    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (list_view), path, NULL,
                                  TRUE,
				  0.5, 0.0);
    gtk_tree_path_free (path);
  }

  return object;
}

static void
ar_game_chooser_dispose (GObject *object)
{
//   ArGameChooser *chooser = AR_GAME_CHOOSER (object);
//   ArGameChooserPrivate *priv = chooser->priv;


  G_OBJECT_CLASS (ar_game_chooser_parent_class)->dispose (object);
}

static void
ar_game_chooser_finalize (GObject *object)
{
  G_OBJECT_CLASS (ar_game_chooser_parent_class)->finalize (object);
}

static void
ar_game_chooser_set_property (GObject      *object,
                              guint         property_id,
                              const GValue *value,
                              GParamSpec   *pspec)
{
  ArGameChooser *chooser = AR_GAME_CHOOSER (object);
  ArGameChooserPrivate *priv = chooser->priv;

  switch (property_id) {
    case PROP_WINDOW:
      priv->window = g_value_get_object (value);
#if !GTK_CHECK_VERSION (2, 10, 0)
      gtk_window_set_transient_for (GTK_WINDOW (chooser), GTK_WINDOW (priv->window));
#endif
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_game_chooser_class_init (ArGameChooserClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  g_type_class_add_private (klass, sizeof (ArGameChooserPrivate));

  object_class->constructor = ar_game_chooser_constructor;
  object_class->set_property = ar_game_chooser_set_property;
  object_class->dispose = ar_game_chooser_dispose;
  object_class->finalize = ar_game_chooser_finalize;
  
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
#if GTK_CHECK_VERSION (2, 10, 0)
                       "transient-for", window,
#endif
                       "destroy-with-parent", TRUE,
                       "window", window,
                       NULL);
}
