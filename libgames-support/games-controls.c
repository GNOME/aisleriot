/* 
 * Copyright © 2004 Paolo Borelli
 * Copyright © 2007 Christian Persch
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
  KEYNAME_COLUMN,
  EDITABLE_COLUMN,
  KEYCODE_COLUMN,
  DEFAULT_KEYCODE_COLUMN,
  N_COLUMNS
};

static const char *
get_keyname_from_keyval (guint keyval)
{
  char *keyname;

  keyname = gdk_keyval_name (keyval);
  if (!keyname)
    keyname = N_("No key");

  return keyname;
}

/*
 * Hackish: we don't want real editing, since it would allow to input
 * a whole string, we just want to grab a key and display the key name.
 * To do this we connect this function to the key_press_event of the
 * TreeView, but if the flag that we set on row_activate is not set
 * we just return.
 */
static gboolean
grab_key (GtkWidget * widget, GdkEventKey * event, GamesControlsList * list)
{
  GtkTreeSelection *selection;
  GtkTreeIter iter;
  gboolean editing;
  char *conf_key;

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (list->view));
  if (!selection)
    return FALSE;

  if (!gtk_tree_selection_get_selected (selection, NULL, &iter))
    return FALSE;

  gtk_tree_model_get (GTK_TREE_MODEL (list->store), &iter,
		      CONFKEY_COLUMN, &conf_key,
		      EDITABLE_COLUMN, &editing,
                      -1);

  if (!editing || !conf_key)
    return FALSE;

  /* just set the editable flag, the key name is updated
   * in the conf notification callback.
   */
  gtk_list_store_set (list->store, &iter, EDITABLE_COLUMN, FALSE, -1);

  games_conf_set_keyval (list->conf_group, conf_key, event->keyval);
  g_free (conf_key);

  return TRUE;
}

static void
abort_grab_key (GamesControlsList * list)
{
  GtkTreeIter iter;
  gboolean valid;

  g_return_if_fail (GAMES_IS_CONTROLS_LIST (list));

  /* find our conf key in the list store and reset it to the previous value */
  valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list->store), &iter);
  while (valid) {
    gboolean editable;
    guint keyval;

    gtk_tree_model_get (GTK_TREE_MODEL (list->store), &iter,
			KEYCODE_COLUMN, &keyval,
			EDITABLE_COLUMN, &editable,
                        -1);

    if (editable) {
      const char *keyname;

      keyname = get_keyname_from_keyval (keyval);

      gtk_list_store_set (list->store, &iter,
			  KEYNAME_COLUMN, keyname,
			  EDITABLE_COLUMN, FALSE,
                          -1);
      break;
    }

    valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (list->store), &iter);
  }
}

static gboolean
abort_on_focus_out_event (GtkWidget * widget,
			  GdkEvent * event, GamesControlsList * list)
{
  abort_grab_key (list);

  return FALSE;
}

static gboolean
abort_on_button_press_event (GtkWidget * widget,
			     GdkEventButton * event, GamesControlsList * list)
{
  abort_grab_key (list);

  return FALSE;
}

static void
control_row_activated (GtkTreeView * view,
		       GtkTreePath * path,
		       GtkTreeViewColumn * tree_view_column,
		       GamesControlsList * list)
{
  GtkTreeIter iter;

  gtk_tree_model_get_iter (GTK_TREE_MODEL (list->store), &iter, path);

  /* set a boolean flag, so that in grab_key we know if we
   * are assigning a new key.
   */
  gtk_list_store_set (list->store, &iter,
		      KEYNAME_COLUMN, _("<Press a Key>"),
		      EDITABLE_COLUMN, TRUE,
                      -1);
}

static void
conf_value_changed_cb (GamesConf *conf,
                       const char *group,
                       const char *key,
                       GamesControlsList *list)
{
  GtkTreeIter iter;
  gboolean valid;
  guint keyval, default_keyval;
  char *conf_key;
  const char *keyname;

  if ((group == NULL && list->conf_group != NULL) ||
      (group != NULL && (list->conf_group == NULL ||
                         strcmp (group, list->conf_group) != 0)))
    return;

  /* find our gconf key in the list store and update it */
  valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list->store), &iter);
  while (valid) {
    gtk_tree_model_get (GTK_TREE_MODEL (list->store), &iter,
                        CONFKEY_COLUMN, &conf_key,
                        -1);

    if (strcmp (key, conf_key) == 0) {
      gtk_tree_model_get (GTK_TREE_MODEL (list->store), &iter,
                          DEFAULT_KEYCODE_COLUMN, &default_keyval,
                          -1);

      keyval = games_conf_get_keyval_with_default (list->conf_group, key, default_keyval);
      keyname = get_keyname_from_keyval (keyval);

      gtk_list_store_set (list->store, &iter,
                          KEYNAME_COLUMN, keyname,
                          KEYCODE_COLUMN, keyval,
                          EDITABLE_COLUMN, FALSE,
                          -1);
      break;
    }

    valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (list->store), &iter);
  }
}

/* Class implementation */

G_DEFINE_TYPE (GamesControlsList, games_controls_list, GTK_TYPE_VBOX)

static void
games_controls_list_init (GamesControlsList * list)
{
  GtkTreeViewColumn *column;
  GtkCellRenderer *label_renderer, *key_renderer;
  GtkWidget *scroll;

  list->store = gtk_list_store_new (N_COLUMNS,
				    G_TYPE_STRING,
				    G_TYPE_STRING,
				    G_TYPE_STRING,
				    G_TYPE_BOOLEAN,
                                    G_TYPE_UINT,
                                    G_TYPE_UINT);
  list->view = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list->store));
  g_object_unref (list->store);

  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (list->view), FALSE);
  gtk_tree_view_set_enable_search (GTK_TREE_VIEW (list->view), FALSE);

  /* label column */
  label_renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes ("Control",
						     label_renderer,
						     "text", LABEL_COLUMN,
						     NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (list->view), column);

  /* key column */
  key_renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes ("Key",
						     key_renderer,
						     "text", KEYNAME_COLUMN,
						     FALSE, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (list->view), column);

  scroll = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scroll),
				       GTK_SHADOW_IN);

  gtk_container_add (GTK_CONTAINER (scroll), list->view);

  gtk_box_pack_start (GTK_BOX (list), scroll, TRUE, TRUE, 0);

  /* when a row is activated we set a flag to know that we are
   * assigning a new key and then we check that flag during the
   * key_press handler.
   */
  g_signal_connect (list->view, "row-activated",
		    G_CALLBACK (control_row_activated), list);
  g_signal_connect (list->view, "key-press-event",
		    G_CALLBACK (grab_key), list);

  /* on focus_out abort the change of the control key */
  g_signal_connect (list->view, "focus-out-event",
		    G_CALLBACK (abort_on_focus_out_event), list);
  g_signal_connect (list->view, "button-press-event",
		    G_CALLBACK (abort_on_button_press_event), list);

  list->notify_handler_id = g_signal_connect (games_conf_get_default (),
                                              "value-changed",
                                              G_CALLBACK (conf_value_changed_cb),
                                              list);
}

static void
games_controls_list_finalize (GObject *object)
        
{
  GamesControlsList *list = GAMES_CONTROLS_LIST (object);

  g_signal_handler_disconnect (games_conf_get_default (), list->notify_handler_id);

  g_free (list->conf_group);

  list->store = NULL;

  G_OBJECT_CLASS (games_controls_list_parent_class)->finalize (object);
}

static void
games_controls_list_class_init (GamesControlsListClass * class)
{
  GObjectClass *oclass = G_OBJECT_CLASS (class);

  oclass->finalize = games_controls_list_finalize;
}

/* Public API */

GtkWidget *
games_controls_list_new (const char *conf_group)
{
  GamesControlsList *list;

  list = g_object_new (GAMES_TYPE_CONTROLS_LIST, NULL);

  list->conf_group = g_strdup (conf_group);

  return GTK_WIDGET (list);
}

void
games_controls_list_add_control (GamesControlsList * list,
				 const gchar * conf_key,
                                 const gchar * label,
                                 guint default_keyval)
{
  GtkTreeIter iter;
  const char *keyname;
  guint keyval;

  g_return_if_fail (GAMES_IS_CONTROLS_LIST (list));
  g_return_if_fail (conf_key != NULL);

  if (!label)
    label = _("Unknown Command");

  keyval = games_conf_get_keyval_with_default (list->conf_group, conf_key, default_keyval);
  keyname = get_keyname_from_keyval (keyval);

  gtk_list_store_append (list->store, &iter);
  gtk_list_store_set (list->store, &iter,
		      CONFKEY_COLUMN, conf_key,
		      LABEL_COLUMN, label,
		      KEYNAME_COLUMN, keyname,
		      EDITABLE_COLUMN, FALSE,
		      KEYCODE_COLUMN, keyval,
                      DEFAULT_KEYCODE_COLUMN, default_keyval,
                      -1);
}

void
games_controls_list_add_controls (GamesControlsList * list,
				  const gchar * first_gconf_key, ...)
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
