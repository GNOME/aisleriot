/* 
 * Copyright © 2004 Paolo Borelli
 * Copyright © 2007, 2009 Christian Persch
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

#include <string.h>

#include "config.h"

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "games-conf.h"
#include "games-controls.h"

enum {
  CONFKEY_COLUMN = 0,
  LABEL_COLUMN,
  KEYCODE_COLUMN,
  KEYMODS_COLUMN,
  DEFAULT_KEYCODE_COLUMN,
  DEFAULT_KEYMODS_COLUMN,
  N_COLUMNS
};

/* Class implementation */

G_DEFINE_TYPE (GamesControlsList, games_controls_list, GTK_TYPE_SCROLLED_WINDOW)

struct GamesControlsListPrivate {
  GtkTreeModel *model;
  GtkListStore *store;
  GtkWidget *view;

  char *conf_group;
  gulong notify_handler_id;
};

static void
accel_edited_cb (GtkCellRendererAccel *cell,
                 char *path_string,
                 guint keyval,
                 GdkModifierType mask,
                 guint hardware_keycode,
                 GamesControlsList *list)
{
  GtkTreePath *path;
  GtkTreeIter iter;
  char *conf_key = NULL;

  path = gtk_tree_path_new_from_string (path_string);
  if (!path)
    return;

  if (!gtk_tree_model_get_iter (list->priv->model, &iter, path)) {
    gtk_tree_path_free (path);
    return;
  }
  gtk_tree_path_free (path);

  gtk_tree_model_get (list->priv->model, &iter,
                      CONFKEY_COLUMN, &conf_key,
                      -1);
  if (!conf_key)
    return;

  /* Note: the model is updated in the conf notification callback */
  /* FIXME: what to do with the modifiers? */
  games_conf_set_keyval (list->priv->conf_group, conf_key, keyval);
  g_free (conf_key);
}

static void
accel_cleared_cb (GtkCellRendererAccel *cell,
                  char *path_string,
                  GamesControlsList *list)
{
  GtkTreePath *path;
  GtkTreeIter iter;
  char *conf_key = NULL;
  guint default_keyval;

  path = gtk_tree_path_new_from_string (path_string);
  if (!path)
    return;

  if (!gtk_tree_model_get_iter (list->priv->model, &iter, path)) {
    gtk_tree_path_free (path);
    return;
  }
  gtk_tree_path_free (path);

  gtk_tree_model_get (list->priv->model, &iter,
                      CONFKEY_COLUMN, &conf_key,
                      DEFAULT_KEYCODE_COLUMN, &default_keyval,
                      -1);
  if (!conf_key)
    return;

  /* Note: the model is updated in the conf notification callback */
  /* FIXME: what to do with the modifiers? */
  games_conf_set_keyval (list->priv->conf_group, conf_key, default_keyval);
  g_free (conf_key);
}

static void
conf_value_changed_cb (GamesConf *conf,
                       const char *group,
                       const char *key,
                       GamesControlsList *list)
{
  GtkTreeIter iter;
  gboolean valid;

  if ((group == NULL && list->priv->conf_group != NULL) ||
      (group != NULL && (list->priv->conf_group == NULL ||
                         strcmp (group, list->priv->conf_group) != 0)))
    return;

  /* find our gconf key in the list store and update it */
  valid = gtk_tree_model_get_iter_first (list->priv->model, &iter);
  while (valid) {
    char *conf_key;

    gtk_tree_model_get (list->priv->model, &iter,
                        CONFKEY_COLUMN, &conf_key,
                        -1);

    if (strcmp (key, conf_key) == 0) {
      guint keyval, default_keyval;

      gtk_tree_model_get (list->priv->model, &iter,
                          DEFAULT_KEYCODE_COLUMN, &default_keyval,
                          -1);

      keyval = games_conf_get_keyval_with_default (list->priv->conf_group, key, default_keyval);

      gtk_list_store_set (list->priv->store, &iter,
                          KEYCODE_COLUMN, keyval,
                          KEYMODS_COLUMN, 0 /* FIXME? */,
                          -1);

      g_free (conf_key);
      break;
    }

    g_free (conf_key);
    valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (list->priv->store), &iter);
  }
}

static void
games_controls_list_init (GamesControlsList *list)
{
  list->priv = G_TYPE_INSTANCE_GET_PRIVATE (list, GAMES_TYPE_CONTROLS_LIST, GamesControlsListPrivate);
}

static GObject *
games_controls_list_constructor (GType type,
                                 guint n_construct_properties,
                                 GObjectConstructParam *construct_params)
{
  GObject *object;
  GamesControlsList *list;
  GtkScrolledWindow *scrolled_window;
  GtkTreeViewColumn *column;
  GtkCellRenderer *label_renderer, *key_renderer;
  GtkListStore *store;

  object = G_OBJECT_CLASS (games_controls_list_parent_class)->constructor
             (type, n_construct_properties, construct_params);

  list = GAMES_CONTROLS_LIST (object);
  scrolled_window = GTK_SCROLLED_WINDOW (object);

  store = gtk_list_store_new (N_COLUMNS,
                              G_TYPE_STRING,
                              G_TYPE_STRING,
                              G_TYPE_UINT,
                              G_TYPE_UINT,
                              G_TYPE_UINT,
                              G_TYPE_UINT);
  list->priv->store = store;
  list->priv->model = GTK_TREE_MODEL (store);

  list->priv->view = gtk_tree_view_new_with_model (list->priv->model);
  g_object_unref (store);

  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (list->priv->view), FALSE);
  gtk_tree_view_set_enable_search (GTK_TREE_VIEW (list->priv->view), FALSE);

  /* label column */
  label_renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes ("Control",
						     label_renderer,
						     "text", LABEL_COLUMN,
						     NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (list->priv->view), column);

  /* key column */
  key_renderer = gtk_cell_renderer_accel_new ();
  g_object_set (key_renderer,
                "editable", TRUE,
                "accel-mode", GTK_CELL_RENDERER_ACCEL_MODE_OTHER,
                NULL);
  g_signal_connect (key_renderer, "accel-edited",
                    G_CALLBACK (accel_edited_cb), list);
  g_signal_connect (key_renderer, "accel-cleared",
                    G_CALLBACK (accel_cleared_cb), list);

  column = gtk_tree_view_column_new_with_attributes ("Key",
						     key_renderer,
                                                     "accel-key", KEYCODE_COLUMN,
                                                     "accel-mods", KEYMODS_COLUMN,
						     NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (list->priv->view), column);

  gtk_container_add (GTK_CONTAINER (scrolled_window), list->priv->view);

  list->priv->notify_handler_id = g_signal_connect (games_conf_get_default (),
                                              "value-changed",
                                              G_CALLBACK (conf_value_changed_cb),
                                              list);

  return object;
}

static void
games_controls_list_finalize (GObject *object)
{
  GamesControlsList *list = GAMES_CONTROLS_LIST (object);

  g_signal_handler_disconnect (games_conf_get_default (), list->priv->notify_handler_id);

  g_free (list->priv->conf_group);

  G_OBJECT_CLASS (games_controls_list_parent_class)->finalize (object);
}

static void
games_controls_list_class_init (GamesControlsListClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->constructor = games_controls_list_constructor;
  gobject_class->finalize = games_controls_list_finalize;

  g_type_class_add_private (gobject_class, sizeof (GamesControlsListPrivate));
}

/* Public API */

GtkWidget *
games_controls_list_new (const char *conf_group)
{
  GamesControlsList *list;

  list = g_object_new (GAMES_TYPE_CONTROLS_LIST,
                       "hscrollbar-policy", GTK_POLICY_NEVER,
                       "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                       "shadow-type", GTK_SHADOW_IN,
                       NULL);

  list->priv->conf_group = g_strdup (conf_group);

  return GTK_WIDGET (list);
}

void
games_controls_list_add_control (GamesControlsList *list,
                                 const char *conf_key,
                                 const char *label,
                                 guint default_keyval)
{
  GtkTreeIter iter;
  guint keyval;

  g_return_if_fail (GAMES_IS_CONTROLS_LIST (list));
  g_return_if_fail (conf_key != NULL);

  if (!label)
    label = _("Unknown Command");

  keyval = games_conf_get_keyval_with_default (list->priv->conf_group, conf_key, default_keyval);

  gtk_list_store_insert_with_values (list->priv->store, &iter, -1,
                                     CONFKEY_COLUMN, conf_key,
                                     LABEL_COLUMN, label,
                                     KEYCODE_COLUMN, keyval,
                                     KEYMODS_COLUMN, 0,
                                     DEFAULT_KEYCODE_COLUMN, default_keyval,
                                     DEFAULT_KEYMODS_COLUMN, 0,
                                     -1);
}

void
games_controls_list_add_controls (GamesControlsList *list,
                                  const char *first_gconf_key,
                                  ...)
{
  va_list args;
  const char *key;
  const char *label;
  guint keyval;

  g_return_if_fail (GAMES_IS_CONTROLS_LIST (list));
  g_return_if_fail (first_gconf_key != NULL);

  va_start (args, first_gconf_key);

  key = first_gconf_key;
  while (key) {
    label = va_arg (args, gchar *);
    keyval = va_arg (args, guint);

    games_controls_list_add_control (list, key, label, keyval);

    key = va_arg (args, gchar *);
  }

  va_end (args);
}

#if 0				/* possible TODO stuff */

-add a "Reset to default" button which resets each command to the defaut key
  (the default can be obtained with gconf_client_get_default_from_schema)

  - add a "Change" button which activates the currently selected row
#endif
