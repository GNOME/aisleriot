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
#include <gnome.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>
#include "games-network.h"
#include "games-network-dialog.h"

static gboolean connect_cb (GtkWidget *widget);
static gboolean start_cb (GtkWidget *widget);
static gboolean close_cb (GtkWidget *widget);


static GtkWidget *game_dialog = NULL;
static GtkWidget *connect_cmd, *cancel_cmd, *start_cmd, *status, *frame;
static GtkWidget *sw, *view, *hbox, *label, *name_entry;

static GtkListStore *model;   
static GtkCellRenderer *rend;

/* FIXME: Another external symbol we don't neeed. */
void gui_message (gchar *message);

void
network_gui_message (const char *message)
{
  gui_message ((char *)message);
  if (status) {
    gtk_label_set_text (GTK_LABEL(status), message);
  }
}

void
network_gui_add_player(const char *name)
{
  GtkTreeIter it;
                                                                                
  gtk_list_store_append (model, &it);
  gtk_list_store_set (model, &it, 0, name , -1);
}
                                                                                
void
network_gui_connected(void)
{
  gtk_widget_hide (connect_cmd);
  gtk_widget_show (start_cmd);
}

void 
network_gui_close(void)
{
  if (game_dialog) {
    gtk_widget_destroy (game_dialog);
  }
  game_dialog = NULL;
  status = NULL;
}

void
network_game_dialog_show (GtkWidget *parent_window)
{
  struct passwd *pwent;

  if (game_dialog) {
    return;
  }

  game_dialog = gtk_dialog_new_with_buttons (_("New Network Game"),
                    			     GTK_WINDOW(parent_window),
				             0, NULL);

  /*  Create Network Status Frame. */
  frame = gtk_frame_new (_("Network Status:"));
  status = gtk_label_new (_("No network game started."));
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (game_dialog)->vbox),
                     frame);
  gtk_container_add (GTK_CONTAINER (frame), status);

  /* Create Players list */
  frame = gtk_frame_new (_("Players:"));
  model = gtk_list_store_new (1, G_TYPE_STRING); 
  view = gtk_tree_view_new_with_model (GTK_TREE_MODEL(model));
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
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (game_dialog)->vbox), frame);

  /* Create playername label and entry field. */
  label = gtk_label_new (_("Your playername:"));
  name_entry = gtk_entry_new ();  
  gtk_entry_set_max_length(GTK_ENTRY(name_entry), 8);

  /* Get the username for this player.  */
  pwent = getpwuid(getuid());
  if (pwent) {
    gtk_entry_set_text (GTK_ENTRY(name_entry), pwent->pw_name);
  } else {
    gtk_entry_set_text (GTK_ENTRY(name_entry), _("Player"));
  }

  hbox = gtk_hbox_new (FALSE, 2);
  gtk_container_add (GTK_CONTAINER(hbox), label);
  gtk_container_add (GTK_CONTAINER(hbox), name_entry);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (game_dialog)->vbox), hbox);

  /* Create buttons. */
  connect_cmd = gtk_button_new_with_mnemonic (_("_Connect"));
  cancel_cmd = gtk_button_new_with_mnemonic (_("_Cancel"));
  start_cmd = gtk_button_new_with_mnemonic (_("_Start game"));

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
  const char *id;
                                                                                                          
  gtk_list_store_clear (model);
  id = gtk_entry_get_text (GTK_ENTRY(name_entry));
  if (!id || *id == '\0') {
    network_gui_message(_("Invalid name"));
    return FALSE;
  }
  network_gui_add_player (id);
  games_network_connect (id);
  return TRUE;
}
                                                                                                          
static gboolean
start_cb (GtkWidget *widget)
{
  games_network_connect (NULL);
  gtk_list_store_clear (model);
  return TRUE;
}

static gboolean
close_cb (GtkWidget *widget)
{
  network_gui_close ();
  return TRUE;
}

