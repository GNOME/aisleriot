/* -*- mode:C; indent-tabs-mode:nil; tab-width:8; c-basic-offset:2; -*- */

/*
 * Games-network-dialog.c - GUI for the common network code. 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * For more details see the file COPYING.
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <pwd.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>
#include <gconf/gconf-client.h>
#include "games-network.h"
#include "games-network-dialog.h"

static gboolean connect_cb (GtkWidget *widget);
static gboolean start_cb (GtkWidget *widget);
static gboolean close_cb (GtkWidget *widget);
static gboolean config_cb (GtkWidget *widget);
static gboolean config_ok_cb (GtkWidget *widget);
static gboolean config_cancel_cb (GtkWidget *widget);
static gboolean config_server_cb (GtkWidget *widget);
static gboolean config_lan_host_cb (GtkWidget *widget);
static gboolean config_lan_join_cb (GtkWidget *widget);

static GtkWidget *game_dialog = NULL, *net_cfg_dialog = NULL;
static GtkWidget *connect_cmd, *cancel_cmd, *start_cmd, *frame;
static GtkWidget *sw, *config_cmd, *status_view, *name_entry, *game_entry;
static GtkWidget *rbutton_a, *rbutton_b, *rbutton_c, *server_entry, *view;
static GtkTextBuffer *status_buf;
static gint network_mode = 2;

static GtkListStore *model;
static GtkCellRenderer *rend;

static GConfClient *conf_client;

/* Callback functions  */
void (*game_msg_cb)(gchar *message);

void
set_game_msg_cb (void (*func)(gchar *message))
{
  game_msg_cb = func;
}

static gboolean
view_scroll (gpointer data)
{
  GtkTextIter iter;

  if (!status_buf || !status_view) {
    return FALSE;
  }

  gtk_text_buffer_get_iter_at_offset (status_buf, &iter, -1);
  gtk_text_view_scroll_to_iter (GTK_TEXT_VIEW(status_view),
			        &iter, 0, TRUE, 0.0, 1.0);

  return FALSE;
}

void
network_gui_message (const char *message)
{
  GtkTextIter iter;

  game_msg_cb ((char *)message);

  if (!status_buf || !status_view) {
    return;
  }

  gtk_text_buffer_get_iter_at_offset (status_buf, &iter, -1);
  gtk_text_buffer_insert (status_buf, &iter, message, -1);  
  gtk_text_buffer_insert (status_buf, &iter, "\n", -1);
  g_idle_add ((GSourceFunc) view_scroll, NULL );
}

void
network_gui_add_player (const char *name)
{
  static char msgbuf[256];

  snprintf (msgbuf, sizeof (msgbuf), 
            _("The player %s joined the game."), name);
  network_gui_message (msgbuf);
}
                                                                                
void
network_gui_connected (void)
{
  gtk_widget_hide (connect_cmd);
  gtk_widget_show (start_cmd);
  gtk_widget_set_sensitive(GTK_WIDGET(connect_cmd), TRUE);
  gtk_widget_set_sensitive(GTK_WIDGET(config_cmd), FALSE);

}

void 
network_gui_close (void)
{
  if (game_dialog) {
    gtk_widget_destroy (game_dialog);
  }
  game_dialog = NULL;
  status_buf = NULL;
  status_view = NULL;
}

void
network_game_dialog_show (GtkWidget *parent_window)
{
  game_dialog = gtk_dialog_new_with_buttons (_("New Network Game"),
                    			     GTK_WINDOW(parent_window),
				             0, NULL);
  conf_client = gconf_client_get_default ();

  /*  Create Network Status Frame. */
  frame = gtk_frame_new (_("Network Status:"));
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (game_dialog)->vbox), frame);
  status_view = gtk_text_view_new ();
  gtk_text_view_set_editable (GTK_TEXT_VIEW(status_view), FALSE);
  status_buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (status_view));
  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_set_size_request (GTK_WIDGET(status_view), -1, 160);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW(sw),
                                      GTK_SHADOW_ETCHED_IN);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(sw),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (sw), status_view);
  gtk_container_add (GTK_CONTAINER (frame), sw);

  gtk_text_buffer_set_text (status_buf, _("Please start a new network game.\n"), -1); 

  /* Create buttons. */
  config_cmd = gtk_button_new_from_stock (GTK_STOCK_PREFERENCES);
  connect_cmd = gtk_button_new_with_mnemonic (_("_Connect"));
  cancel_cmd = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  start_cmd = gtk_button_new_with_mnemonic (_("_Start game"));

  gtk_dialog_add_action_widget (GTK_DIALOG(game_dialog),
                                   GTK_WIDGET(config_cmd),
                                   1);
  gtk_dialog_add_action_widget (GTK_DIALOG(game_dialog),
                                   GTK_WIDGET(cancel_cmd),
                                   1);
  gtk_dialog_add_action_widget (GTK_DIALOG(game_dialog),
                                   GTK_WIDGET(connect_cmd),
                                   1);
  gtk_widget_show_all (game_dialog);

  gtk_dialog_add_action_widget (GTK_DIALOG(game_dialog),
                                   GTK_WIDGET(start_cmd),
                                   1);

  g_signal_connect (G_OBJECT (config_cmd), "clicked", G_CALLBACK
                    (config_cb), NULL);
  g_signal_connect (G_OBJECT (connect_cmd), "clicked", G_CALLBACK
                    (connect_cb), NULL);
  g_signal_connect (G_OBJECT (cancel_cmd), "clicked", G_CALLBACK
                    (close_cb), NULL);
  g_signal_connect (G_OBJECT (start_cmd), "clicked", G_CALLBACK
                    (start_cb), NULL);
}

static gboolean
connect_cb (GtkWidget *widget)
{
  const char *name, *server;
                                                                                                          
  gtk_widget_set_sensitive(GTK_WIDGET(connect_cmd), FALSE);
  gtk_widget_set_sensitive(GTK_WIDGET(config_cmd), FALSE);

  name  = gconf_client_get_string (conf_client, KEY_NETWORK_NAME, NULL);

  if (!name || *name == '\0') {
    network_gui_message(_("Invalid name"));
    return FALSE;
  }

  server = gconf_client_get_string (conf_client, KEY_NETWORK_SERVER, NULL);

  if (server == NULL) {
    server = game_server;
  }

  if (network_mode == SERVER_MODE) {
    games_network_connect (server, name);
  } else if (network_mode == HOST_LAN_MODE) {
    char *gamename = gconf_client_get_string (conf_client,
				 KEY_NETWORK_GAMENAME, NULL);
    if (games_host_lan_game ((char *)gamename)) {
      network_gui_message (_("A new gnome games server was successfully started."));
    } else {
      gtk_widget_set_sensitive(GTK_WIDGET(connect_cmd), TRUE);
      gtk_widget_set_sensitive(GTK_WIDGET(config_cmd), TRUE);
    }
  } else if (network_mode == JOIN_LAN_MODE) {
    games_network_connect (server, name);

  }


  return TRUE;
}
                                                                                                          
static gboolean
start_cb (GtkWidget *widget)
{
  games_network_connect (NULL, NULL);
  return TRUE;
}

static gboolean
close_cb (GtkWidget *widget)
{
  network_gui_close ();
  games_network_stop ();
  return TRUE;
}

static gboolean
config_cb (GtkWidget *widget)
{
  GtkWidget *label, *hbox, *ok_cmd, *cancel_cmd, *table, *frame;
  static GtkWidget *sw;

  struct passwd *pwent;
  const char *name, *server, *gamename;


  if (net_cfg_dialog) {
    return TRUE;
  }

  net_cfg_dialog = gtk_dialog_new_with_buttons (_("Network Configuration"),
                                             GTK_WINDOW(game_dialog),
                                             0, NULL);
  frame = gtk_frame_new (NULL);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (net_cfg_dialog)->vbox), frame);

  label = gtk_label_new(_("Your playername:"));

  name_entry = gtk_entry_new ();
  gtk_entry_set_max_length(GTK_ENTRY(name_entry), 12);

  name = gconf_client_get_string (conf_client, KEY_NETWORK_NAME, NULL);

  if (name == NULL) {
    pwent = getpwuid(getuid());
    if (pwent) {
      gtk_entry_set_text (GTK_ENTRY(name_entry), pwent->pw_name);
    } else {
      gtk_entry_set_text (GTK_ENTRY(name_entry), _("Player"));
    }
  } else {
    gtk_entry_set_text (GTK_ENTRY(name_entry), name);
  }

  hbox = gtk_hbox_new (FALSE, 2);
  gtk_container_add (GTK_CONTAINER(hbox), label);
  gtk_container_add (GTK_CONTAINER(hbox), name_entry);
  gtk_container_add (GTK_CONTAINER(frame), hbox);


  frame = gtk_frame_new (_("Network connection method"));
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (net_cfg_dialog)->vbox), frame);
  table = gtk_table_new (3, 2, FALSE);
  gtk_container_add (GTK_CONTAINER (frame), table);

  rbutton_a = gtk_radio_button_new_with_label (NULL, _("Gnome Games Server"));
  gtk_table_attach (GTK_TABLE(table), rbutton_a, 1, 2, 1, 2,
                    GTK_FILL, GTK_FILL, 2, 2);

  rbutton_b = gtk_radio_button_new_with_label_from_widget (
			GTK_RADIO_BUTTON (rbutton_a),
			_("Host Local Game"));
#ifdef NO_HOWL
  gtk_widget_set_sensitive (rbutton_b, FALSE);
#endif
  gtk_table_attach (GTK_TABLE(table), rbutton_b, 1, 2, 2, 3,
                    GTK_FILL, GTK_FILL, 2, 2);
  game_entry = gtk_entry_new ();
#ifdef NO_HOWL
  gtk_widget_set_sensitive (game_entry, FALSE);
#endif
  gtk_entry_set_max_length(GTK_ENTRY(game_entry), 12);
  gamename = gconf_client_get_string (conf_client, KEY_NETWORK_GAMENAME, NULL);

  if (gamename == NULL) {
    gtk_entry_set_text (GTK_ENTRY(game_entry), "gnomegames");
  } else {
    gtk_entry_set_text (GTK_ENTRY(game_entry), gamename);
  }
  frame = gtk_frame_new (_("Game name:"));
#ifdef NO_HOWL
  gtk_widget_set_sensitive (frame, FALSE);
#endif
  gtk_container_add (GTK_CONTAINER(frame), game_entry);
  gtk_table_attach (GTK_TABLE(table), frame, 2, 3, 2, 3,
                    GTK_FILL, GTK_FILL, 2, 2);


  rbutton_c = gtk_radio_button_new_with_label_from_widget (
                        GTK_RADIO_BUTTON (rbutton_b),
                        _("Join Local Game"));
#ifdef NO_HOWL
  gtk_widget_set_sensitive (rbutton_c, FALSE);
#endif
  gtk_table_attach (GTK_TABLE(table), rbutton_c, 1, 2, 3, 4,
                    GTK_FILL, GTK_FILL, 2, 2);


  g_signal_connect (G_OBJECT (rbutton_a), "clicked", G_CALLBACK
                    (config_server_cb), NULL);
  g_signal_connect (G_OBJECT (rbutton_b), "clicked", G_CALLBACK
                    (config_lan_host_cb), NULL);
  g_signal_connect (G_OBJECT (rbutton_c), "clicked", G_CALLBACK
                    (config_lan_join_cb), NULL);

  server_entry = gtk_entry_new ();

  frame = gtk_frame_new (_("Hostname:"));
  gtk_container_add (GTK_CONTAINER(frame), server_entry);
  gtk_table_attach (GTK_TABLE(table), frame, 2, 3, 1, 2,
                    GTK_FILL, GTK_FILL, 2, 2);

  server = gconf_client_get_string (conf_client, KEY_NETWORK_SERVER, NULL);
  if (server == NULL) {
    gtk_entry_set_text (GTK_ENTRY(server_entry), game_server);
  } else {
    gtk_entry_set_text (GTK_ENTRY(server_entry), server);
  }

  frame = gtk_frame_new (_("Local Games:"));
#ifdef NO_HOWL
  gtk_widget_set_sensitive (frame, FALSE);
#endif
  model = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
  view = gtk_tree_view_new_with_model (GTK_TREE_MODEL(model));
#ifdef NO_HOWL
  gtk_widget_set_sensitive (view, FALSE);
#endif
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(view), FALSE);
  rend = gtk_cell_renderer_text_new ();
  g_object_set (rend, "weight", "bold", NULL);
  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW(view),
                                              -1, NULL, rend, "text", 0, NULL);
  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW(sw),
                                      GTK_SHADOW_ETCHED_IN);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(sw),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER(sw), view);
  gtk_container_add (GTK_CONTAINER(frame), sw);
  gtk_table_attach (GTK_TABLE(table), frame, 2, 3, 3, 4,
                    GTK_FILL, GTK_FILL, 2, 2);

  network_mode = gconf_client_get_int (conf_client, KEY_NETWORK_MODE, NULL);
  if (network_mode == SERVER_MODE || network_mode == 0) {
    gtk_button_clicked (GTK_BUTTON(rbutton_a));
  } else if (network_mode == HOST_LAN_MODE ) {
    gtk_button_clicked (GTK_BUTTON(rbutton_b));
  } else {
    gtk_button_clicked (GTK_BUTTON(rbutton_c));
    gtk_list_store_clear (model);
    games_find_lan_game ();
  }


  /* Create buttons */
  cancel_cmd = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  ok_cmd = gtk_button_new_from_stock (GTK_STOCK_OK);

  gtk_dialog_add_action_widget (GTK_DIALOG(net_cfg_dialog),
                                   GTK_WIDGET(cancel_cmd),
                                   1);
  gtk_dialog_add_action_widget (GTK_DIALOG(net_cfg_dialog),
                                   GTK_WIDGET(ok_cmd),
                                   1);

  g_signal_connect (G_OBJECT (ok_cmd), "clicked", G_CALLBACK
                    (config_ok_cb), NULL);
  g_signal_connect (G_OBJECT (cancel_cmd), "clicked", G_CALLBACK
                    (config_cancel_cb), NULL);

  gtk_widget_show_all (net_cfg_dialog);

  return TRUE;
}

static gboolean
config_ok_cb (GtkWidget *widget)
{
  const char *name, *server, *gamename;
  GtkTreeIter it;
  GtkTreeSelection *selection;
  char *lan_server;

  name = gtk_entry_get_text (GTK_ENTRY(name_entry));
  gconf_client_set_string (conf_client, KEY_NETWORK_NAME, name, NULL);
  gamename = gtk_entry_get_text (GTK_ENTRY(game_entry));
  gconf_client_set_string (conf_client, KEY_NETWORK_GAMENAME, gamename, NULL);

  if (network_mode == SERVER_MODE || network_mode == 0) {
    server = gtk_entry_get_text (GTK_ENTRY(server_entry));
    gconf_client_set_string (conf_client, KEY_NETWORK_SERVER, server, NULL);

  } else if (network_mode == JOIN_LAN_MODE){
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
    if (!gtk_tree_selection_get_selected(selection, NULL, &it)) {
      GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(net_cfg_dialog),
                  	                GTK_DIALOG_DESTROY_WITH_PARENT,
                               		GTK_MESSAGE_ERROR,
                               		GTK_BUTTONS_CLOSE,
                       		        _("No local game selected."));
      gtk_dialog_run (GTK_DIALOG (dialog));
      gtk_widget_destroy (dialog);
      return TRUE;
    }
    gtk_tree_model_get(GTK_TREE_MODEL(model), &it, 1, &lan_server, -1);
    if (lan_server != NULL) {
      gconf_client_set_string (conf_client, KEY_NETWORK_SERVER, (char *)lan_server, NULL);
    } 

  }

  gconf_client_set_int (conf_client, KEY_NETWORK_MODE, network_mode, NULL);

  gtk_widget_destroy (net_cfg_dialog);
  net_cfg_dialog = NULL;
  
  return TRUE;
}

static gboolean
config_cancel_cb (GtkWidget *widget)
{
  gtk_widget_destroy (net_cfg_dialog);
  net_cfg_dialog = NULL;

  return TRUE;
}


static gboolean
config_server_cb (GtkWidget *widget)
{
  network_mode = SERVER_MODE; 

  return TRUE;
}

static gboolean
config_lan_host_cb (GtkWidget *widget)
{
  network_mode = HOST_LAN_MODE;
  return TRUE;
}

static gboolean
config_lan_join_cb (GtkWidget *widget)
{
  network_mode = JOIN_LAN_MODE;
  gtk_list_store_clear (model);
  games_find_lan_game ();
  return TRUE;
}

void
network_gui_add_server (char *name, char *address)
{
  GtkTreeIter it;

  gtk_list_store_append (model, &it);
  gtk_list_store_set (model, &it, 0, name , 1, address, -1);
}

