/* 
 * Copyright (C) 2004 Paolo Borelli
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

#include <glib/gi18n-lib.h>
#include <gtk/gtk.h>
#include <gconf/gconf.h>
#include <gconf/gconf-client.h>

#include "games-controls.h"


/* FIXME: The listeners for gconf events are still listening after
 * the controls_list is destroyed. */

enum
{
	GCONFKEY_COLUMN = 0,
	LABEL_COLUMN,
	KEYNAME_COLUMN,
	EDITABLE_COLUMN,
	KEYCODE_COLUMN,
	TYPE_COLUMN,
	N_COLUMNS
};

static void games_controls_list_dispose (GamesControlsList *list)
{
	GConfClient * client;
	GSList * l;

	if (list->dispose_has_run)
		return;
	
	client = gconf_client_get_default ();
	l = list->notify_list;
	while (l) {
		gconf_client_notify_remove (client,
			GPOINTER_TO_INT (l->data));
		l = l->next;
	}

	list->dispose_has_run = TRUE;
}

static void games_controls_list_finalize (GamesControlsList *list)
{
	g_slist_free (list->notify_list);
}

static void
games_controls_list_class_init (GamesControlsListClass *class)
{
	GObjectClass *oclass = G_OBJECT_CLASS (class);

	oclass->dispose = (GObjectFinalizeFunc)games_controls_list_dispose;
	oclass->finalize = (GObjectFinalizeFunc)games_controls_list_finalize;
}

/*
 * Hackish: we don't want real editing, since it would allow to input
 * a whole string, we just want to grab a key and display the key name.
 * To do this we connect this function to the key_press_event of the
 * TreeView, but if the flag that we set on row_activate is not set
 * we just return.
 */
static gboolean
grab_key (GtkWidget *widget,
          GdkEventKey *event,
          GamesControlsList *list)
{
	GtkTreeSelection *selection;
	GtkTreeIter iter;
	gboolean editing;
	const gchar *gconf_key;
	GConfClient *client;
	GConfValueType type;
	gchar * keyname;

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (list->view));
	if (!selection)
		return FALSE;

	if (!gtk_tree_selection_get_selected (selection, NULL, &iter))
		return FALSE;

	gtk_tree_model_get (GTK_TREE_MODEL (list->store), &iter,
	                    GCONFKEY_COLUMN, &gconf_key,
	                    EDITABLE_COLUMN, &editing, 
			    TYPE_COLUMN, &type, -1);

	if (!editing || !gconf_key)
		return FALSE;

	client = gconf_client_get_default ();
	if (type == GCONF_VALUE_STRING) {
		keyname = gdk_keyval_name (event->keyval);
		gconf_client_set_string (client, gconf_key, keyname, NULL);
	} else if (type == GCONF_VALUE_INT) {
		gconf_client_set_int (client, gconf_key, event->keyval,
		NULL);
	} else
		g_warning ("GConf key not a string or int.\n");
	g_object_unref (G_OBJECT (client));

	/* just set the editable flag, the key name is updated
	 * in the gconf notification callback.
	 */
	gtk_list_store_set (list->store, &iter,
	                    EDITABLE_COLUMN, FALSE, -1);

	return TRUE;
}

static void
abort_grab_key (GamesControlsList *list)
{
	GtkTreeIter iter;
	gboolean valid;

	g_return_if_fail (GAMES_IS_CONTROLS_LIST (list));

	/* find our gconf key in the list store and reset it to the previous value */
	valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list->store), &iter);
	while (valid)
	{
		gboolean editable;
		const gchar *keyname;
		guint keycode;

		gtk_tree_model_get (GTK_TREE_MODEL (list->store), &iter,
		                    KEYCODE_COLUMN, &keycode,
		                    EDITABLE_COLUMN, &editable, -1);

		if (editable)
		{
			keyname = gdk_keyval_name (keycode);
			if (!keyname)
				keyname = N_("No key");

			gtk_list_store_set (list->store, &iter,
			                    KEYNAME_COLUMN, keyname,
			                    EDITABLE_COLUMN, FALSE, -1);
		}

		valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (list->store), &iter);
	}
}

static gboolean
abort_on_focus_out_event (GtkWidget *widget,
		          GdkEvent *event,
                          GamesControlsList *list)
{
	abort_grab_key (list);

	return FALSE;
}

static gboolean
abort_on_button_press_event (GtkWidget *widget,
		             GdkEventButton *event,
                             GamesControlsList *list)
{
	abort_grab_key (list);

	return FALSE;
}

static void
control_row_activated (GtkTreeView *view,
                       GtkTreePath *path,
                       GtkTreeViewColumn *tree_view_column,
                       GamesControlsList *list)
{
	GtkTreeIter iter;

	gtk_tree_model_get_iter (GTK_TREE_MODEL (list->store), &iter, path);

	/* set a boolean flag, so that in grab_key we know if we
	 * are assigning a new key.
	 */
	gtk_list_store_set (list->store, &iter,
	                    KEYNAME_COLUMN, _("<Press a Key>"),
	                    EDITABLE_COLUMN, TRUE, -1);
}

static void
games_controls_list_init (GamesControlsList *list)
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
	g_object_unref (G_OBJECT (list->store));

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
	                                                   FALSE,
	                                                   NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (list->view), column);

	scroll = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll),
	                                GTK_POLICY_AUTOMATIC,
	                                GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scroll),
	                                     GTK_SHADOW_IN);

	gtk_container_add (GTK_CONTAINER (scroll), list->view);

	gtk_box_pack_start (GTK_BOX (list), scroll, TRUE, TRUE, 0);

	/* when a row is activated we set a flag to know that we are
	 * assigning a new key and then we check that flag during the
	 * key_press handler.
	 */
	g_signal_connect (G_OBJECT (list->view), "row_activated",
	                  G_CALLBACK (control_row_activated), list);
	g_signal_connect (G_OBJECT (list->view), "key_press_event",
	                  G_CALLBACK (grab_key), list);

	/* on focus_out abort the change of the control key */
	g_signal_connect (G_OBJECT (list->view), "focus_out_event",
	                  G_CALLBACK (abort_on_focus_out_event), list);
	g_signal_connect (G_OBJECT (list->view), "button_press_event",
	                  G_CALLBACK (abort_on_button_press_event), list);

	list->dispose_has_run = FALSE;
	list->notify_list = NULL;
}

GType
games_controls_list_get_type (void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info = {
			sizeof (GamesControlsListClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) games_controls_list_class_init,
			(GClassFinalizeFunc) NULL,
			NULL,
			sizeof (GamesControlsList),
			0,
			(GInstanceInitFunc) games_controls_list_init
		};

		type = g_type_register_static (GTK_TYPE_VBOX, "GamesControlsList", &info, 0);
	}

	return type;
}

GtkWidget *
games_controls_list_new (void)
{
	return g_object_new (GAMES_TYPE_CONTROLS_LIST, NULL);
}

static void
gconf_key_changed (GConfClient *client,
                   guint cnxn_id,
                   GConfEntry *entry,
                   GamesControlsList *list)
{
	const gchar *gconf_key;
	const gchar *keyname;
	guint keycode;
	GtkTreeIter iter;
	gboolean valid;
	GConfValue *value;

	g_return_if_fail (GAMES_IS_CONTROLS_LIST (list));

	keyname = NULL;
	keycode = 0;
	value = gconf_client_get (client, entry->key, NULL);
	if (value->type == GCONF_VALUE_INT) {
		keycode = gconf_value_get_int (value);
		keyname = gdk_keyval_name (keycode);
	} else if (value->type == GCONF_VALUE_STRING) {
	 	keyname = gconf_value_get_string (value);
		keycode = gdk_keyval_from_name (keyname);
	} 

	if (!keyname)
		keyname = N_("No key");

	/* find our gconf key in the list store and update it */
	valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list->store), &iter);
	while (valid)
	{
		gtk_tree_model_get (GTK_TREE_MODEL (list->store), &iter,
		                    GCONFKEY_COLUMN, &gconf_key, -1);

		if (strcmp (entry->key, gconf_key) == 0)
		{
			gtk_list_store_set (list->store, &iter,
			                    KEYNAME_COLUMN, keyname,
			                    KEYCODE_COLUMN, keycode,
			                    EDITABLE_COLUMN, FALSE, -1);
			break;
		}

		valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (list->store), &iter);
	}

	gconf_value_free (value);
}

void
games_controls_list_add_control (GamesControlsList *list,
                                 const gchar *gconf_key)
{
	GConfClient *client;
	GConfEngine *engine;
	GConfEntry *entry;
	GConfSchema *schema;
	const gchar *label, *keyname;
	guint keycode;
	GtkTreeIter iter;
	GConfValueType type;
	guint cid;
	const gchar * schemaname;

	g_return_if_fail (GAMES_IS_CONTROLS_LIST (list));
	g_return_if_fail (gconf_key != NULL);

	client = gconf_client_get_default ();

	/* We have to go to the root to make sure we can get
	 * the schemas. */
	engine = gconf_engine_get_for_address (GCONF_SCHEMA_CONFIG_SOURCE, NULL);

	entry = gconf_engine_get_entry (engine, gconf_key, NULL, TRUE, NULL);
	g_return_if_fail (entry != NULL);

	schemaname = gconf_entry_get_schema_name (entry);
	schema = gconf_client_get_schema (client,
	                                  schemaname,
	                                  NULL);
	g_return_if_fail (schema != NULL);

	label = gconf_schema_get_short_desc (schema);
	if (!label)
		label = N_("Unknown Command");

	type = gconf_schema_get_type (schema);

	/* The result could be a keycode or a key name. */
	if (type == GCONF_VALUE_INT) {
		keycode = gconf_client_get_int (client, gconf_key, NULL);
		keyname = gdk_keyval_name (keycode);
	} else if (type == GCONF_VALUE_STRING) {
		keyname = gconf_client_get_string (client, gconf_key, NULL);
		keycode = gdk_keyval_from_name (keyname);
	} else return; /* We don't understand anything else. */
	if (!keyname)
		keyname = N_("No key");

	gtk_list_store_append (list->store, &iter);
	gtk_list_store_set (list->store, &iter,
	                    GCONFKEY_COLUMN, gconf_key,
	                    LABEL_COLUMN, label,
	                    KEYNAME_COLUMN, keyname,
	                    EDITABLE_COLUMN, FALSE,
	                    KEYCODE_COLUMN, keycode,
			    TYPE_COLUMN, type,
			    -1);

	cid = gconf_client_notify_add (client, gconf_key,
	      	      (GConfClientNotifyFunc)gconf_key_changed, list, 
		      NULL, NULL);

	list->notify_list = g_slist_prepend (list->notify_list, GINT_TO_POINTER (cid));

	gconf_engine_unref (engine);
	g_object_unref (G_OBJECT (client));
}

void
games_controls_list_add_controls (GamesControlsList *list,
                                  const gchar *first_gconf_key, ...)
{
	va_list args;
	const gchar *key;

	g_return_if_fail (GAMES_IS_CONTROLS_LIST (list));
	g_return_if_fail (first_gconf_key != NULL);

	va_start (args, first_gconf_key);

	key = first_gconf_key;
	while (key)
	{
		games_controls_list_add_control (list, key);

		key = va_arg (args, gchar*);
	}

	va_end (args);
}

#if 0 /* possible TODO stuff */

- add a "Reset to default" button which resets each command to the defaut key
 (the default can be obtained with gconf_client_get_default_from_schema)

- add a "Change" button which activates the currently selected row

#endif

