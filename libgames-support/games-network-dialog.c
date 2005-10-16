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
#include "games-frame.h"
#include "games-network.h"
#include "games-network-dialog.h"

static gboolean connect_cb (GtkWidget *widget);
static gboolean start_cb (GtkWidget *widget);
static gboolean close_cb (GtkWidget *widget);
static gboolean config_server_cb (GtkWidget *widget);
static gboolean config_lan_host_cb (GtkWidget *widget);
static gboolean config_lan_join_cb (GtkWidget *widget);

static GtkWidget *game_dialog = NULL, *net_cfg_dialog = NULL;
static GtkWidget *connect_cmd, *cancel_cmd, *start_cmd, *frame;
static GtkWidget *sw, *status_view, *name_entry, *game_entry;
static GtkWidget *rbutton_a, *rbutton_b, *rbutton_c, *server_entry, *view;
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

void
network_gui_message (const char *message)
{
  gtk_label_set_text (GTK_LABEL (status_view), message);
}

void
network_gui_add_player (const char *name)
{
  static char msgbuf[256];

  snprintf (msgbuf, sizeof (msgbuf), 
            _("The player %s has joined the game."), name);
  network_gui_message (msgbuf);
}
                                                                                
void
network_gui_connected (void)
{
  gtk_widget_hide (connect_cmd);
  gtk_widget_show (start_cmd);
  gtk_widget_set_sensitive(GTK_WIDGET(connect_cmd), TRUE);

}

void 
network_gui_close (void)
{
  if (game_dialog) {
    gtk_widget_destroy (game_dialog);
  }
  game_dialog = NULL;
}

static void 
error_dialog (gchar *message)
{
  GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(net_cfg_dialog),
                                              GTK_DIALOG_DESTROY_WITH_PARENT,
                                              GTK_MESSAGE_ERROR,
                                              GTK_BUTTONS_CLOSE,
                                              message);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

void
network_game_dialog_show (GtkWidget *parent_window)
{
  GtkWidget *vbox;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *vbox2;
  gchar *server;
  gchar *gamename;
  gchar *name;
  struct passwd *pwent;
  
  game_dialog = gtk_dialog_new_with_buttons (_("New Network Game"),
                    			     GTK_WINDOW(parent_window),
				             0, NULL, NULL);
  gtk_dialog_set_has_separator (GTK_DIALOG (game_dialog), FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (game_dialog), 5);
  gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (game_dialog)->vbox), 2);
  conf_client = gconf_client_get_default ();

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (game_dialog)->vbox), vbox);

  /*  Create Network Status Frame. */
  frame = games_frame_new (_("Status"));
  gtk_box_pack_start (GTK_BOX (vbox), frame, TRUE, TRUE, 0);
  status_view = gtk_label_new (_("Select a nickname and connection method then click Connect."));
  gtk_container_add (GTK_CONTAINER (frame), status_view);

  /* Now for the network connection options. */
  frame = games_frame_new (_("Connection Method"));
  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 13);

  vbox2 = gtk_vbox_new (FALSE, 6);
  gtk_container_add (GTK_CONTAINER (frame), vbox2);

  rbutton_a = gtk_radio_button_new_with_label (NULL, _("GNOME games server"));
  gtk_box_pack_start (GTK_BOX(vbox2), rbutton_a, FALSE, FALSE, 0);

  server_entry = gtk_entry_new ();

  hbox = gtk_hbox_new (FALSE, 12);
  label = gtk_label_new ("    ");
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0); 
  label = gtk_label_new_with_mnemonic (_("_Hostname:"));
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), server_entry);
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX(hbox), server_entry, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX(vbox2), hbox, FALSE, FALSE, 0);
    
  server = gconf_client_get_string (conf_client, KEY_NETWORK_SERVER, NULL);
  if (server == NULL) {
    gtk_entry_set_text (GTK_ENTRY(server_entry), games_network_server_name);
  } else {
    gtk_entry_set_text (GTK_ENTRY(server_entry), server);
  }

  rbutton_b = gtk_radio_button_new_with_label_from_widget (
			GTK_RADIO_BUTTON (rbutton_a),
			_("Host local game"));
#ifndef HAVE_ZEROCONF
  gtk_widget_set_sensitive (rbutton_b, FALSE);
#endif
  gtk_box_pack_start (GTK_BOX(vbox2), rbutton_b, FALSE, FALSE, 0);
  game_entry = gtk_entry_new ();
#ifndef HAVE_ZEROCONF
  gtk_widget_set_sensitive (game_entry, FALSE);
#endif
  gtk_entry_set_max_length(GTK_ENTRY(game_entry), 12);
  gamename = gconf_client_get_string (conf_client, KEY_NETWORK_GAMENAME, NULL);

  if (gamename == NULL) {
    gtk_entry_set_text (GTK_ENTRY(game_entry), "gnomegames");
  } else {
    gtk_entry_set_text (GTK_ENTRY(game_entry), gamename);
  }
  hbox = gtk_hbox_new (FALSE, 12);
  label = gtk_label_new ("    ");
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);  
  label = gtk_label_new_with_mnemonic (_("_Game name:"));
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), game_entry);
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);
#ifndef HAVE_ZEROCONF
  gtk_widget_set_sensitive (label, FALSE);
  gtk_widget_set_sensitive (game_entry, FALSE);
#endif
  gtk_box_pack_start (GTK_BOX(hbox), game_entry, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX(vbox2), hbox, FALSE, FALSE, 0);

  rbutton_c = gtk_radio_button_new_with_label_from_widget (
                        GTK_RADIO_BUTTON (rbutton_b),
                        _("Join local game"));
#ifndef HAVE_ZEROCONF
  gtk_widget_set_sensitive (rbutton_c, FALSE);
#endif
  gtk_box_pack_start (GTK_BOX(vbox2), rbutton_c, FALSE, FALSE, 0);

  g_signal_connect (G_OBJECT (rbutton_a), "clicked", G_CALLBACK
                    (config_server_cb), NULL);
  g_signal_connect (G_OBJECT (rbutton_b), "clicked", G_CALLBACK
                    (config_lan_host_cb), NULL);
  g_signal_connect (G_OBJECT (rbutton_c), "clicked", G_CALLBACK
                    (config_lan_join_cb), NULL);

  hbox = gtk_hbox_new (FALSE, 12);
  label = gtk_label_new ("    ");
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0); 
  label = gtk_label_new_with_mnemonic (_("_Local games:"));
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX(vbox2), hbox, FALSE, FALSE, 0);
  
#ifndef HAVE_ZEROCONF
  gtk_widget_set_sensitive (label, FALSE);
#endif
  model = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
  view = gtk_tree_view_new_with_model (GTK_TREE_MODEL(model));
#ifndef HAVE_ZEROCONF
  gtk_widget_set_sensitive (view, FALSE);
#endif
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(view), FALSE);
  rend = gtk_cell_renderer_text_new ();
  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW(view),
                                              -1, NULL, rend, "text", 0, NULL);
  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW(sw),
                                      GTK_SHADOW_ETCHED_IN);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(sw),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER(sw), view);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), sw);
  
  hbox = gtk_hbox_new (FALSE, 12);
  label = gtk_label_new ("    ");
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX(hbox), sw, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX(vbox2), hbox, TRUE, TRUE, 0);

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

  /* Add the players nickname entry. */
  frame = games_frame_new ("Identification");

  label = gtk_label_new_with_mnemonic(_("_Nickname:"));

  name_entry = gtk_entry_new ();
  gtk_entry_set_max_length(GTK_ENTRY(name_entry), 12);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), name_entry);

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

  hbox = gtk_hbox_new (FALSE, 12);
  gtk_container_add (GTK_CONTAINER (frame), hbox);

  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX(hbox), name_entry, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX(vbox), frame, FALSE, FALSE, 0);

  /* Create buttons. */
  connect_cmd = gtk_button_new_with_mnemonic (_("C_onnect"));
  cancel_cmd = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  start_cmd = gtk_button_new_with_mnemonic (_("_Start Game"));

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
  const gchar *name;
  const gchar *server = NULL;
  const gchar *gamename;
  GtkTreeIter it;
  GtkTreeSelection *selection;
  char *lan_server;
             
  name = gtk_entry_get_text (GTK_ENTRY(name_entry));
  gconf_client_set_string (conf_client, KEY_NETWORK_NAME, name, NULL);

  if (!name || *name == '\0') {
    error_dialog (_("Please supply a nickname."));
    return FALSE;
  }

  gamename = gtk_entry_get_text (GTK_ENTRY(game_entry));
  gconf_client_set_string (conf_client, KEY_NETWORK_GAMENAME, gamename, NULL);

  if (network_mode == SERVER_MODE || network_mode == 0) {
    server = gtk_entry_get_text (GTK_ENTRY(server_entry));
    gconf_client_set_string (conf_client, KEY_NETWORK_SERVER, server, NULL);

  } else if (network_mode == JOIN_LAN_MODE){
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
    if (!gtk_tree_selection_get_selected(selection, NULL, &it)) {
      error_dialog (_("No local game selected."));
      return FALSE;
    }
    gtk_tree_model_get(GTK_TREE_MODEL(model), &it, 1, &lan_server, -1);
    if (lan_server != NULL) {
      gconf_client_set_string (conf_client, KEY_NETWORK_SERVER, (char *)lan_server, NULL);
    } 

  }

  gconf_client_set_int (conf_client, KEY_NETWORK_MODE, network_mode, NULL);

  if (server == NULL) {
    server = games_network_server_name;
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

