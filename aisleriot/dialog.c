/* AisleRiot - dialog.c
 * Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <guile/gh.h>
#include <dirent.h>
#include "gnome.h"
#include "cscmi.h"
#include "sol.h"
#include "menu.h"
#include "dialog.h"
#include "draw.h"
#include "events.h"

static GtkWidget *hint_dlg = NULL;

void show_game_over_dialog() {
  GtkWidget* dialog;
  gchar* message;

  if (game_won)
    message = g_strdup_printf ("<b>%s\n\n%s</b>", _("Congratulations."),
                                _("You won!!!"));
  else
    message = g_strdup_printf ("<b>%s</b>", _("Game over."));

  dialog = gtk_message_dialog_new (GTK_WINDOW (app),
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_NONE,
                                   message);

  g_free (message);
  gtk_label_set_use_markup (GTK_LABEL (GTK_MESSAGE_DIALOG (dialog)->label),
                            TRUE);
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          _("New Game"), GTK_RESPONSE_ACCEPT,
                          GTK_STOCK_QUIT, GTK_RESPONSE_REJECT,
                          NULL);
  gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);

  /* add a stock icon? */ 
  switch (gtk_dialog_run (GTK_DIALOG (dialog)))
  {
    case GTK_RESPONSE_ACCEPT: 
      {
        gtk_widget_destroy (dialog);
        new_game (NULL, NULL);
      }
      break;
    case GTK_RESPONSE_REJECT:
      {
        gtk_widget_destroy (dialog);
        gtk_widget_destroy (app);
        gtk_main_quit ();
      }
      break;
    default:
     {
       gtk_widget_destroy (dialog);
     }
  }
}

gchar *filename = NULL;

void select_rules(GtkTreeSelection *select, gpointer data)
{
	GtkTreeModel *model;
        GtkTreeIter iter;

	gtk_tree_selection_get_selected(select, &model, &iter);
	gtk_tree_model_get(model, &iter, 1, &filename, -1);
}

void select_game (GtkWidget *app, gint response, GtkWidget* entry)
{
  if(response == GTK_RESPONSE_OK) {
    seed = atoi (gtk_entry_get_text (GTK_ENTRY (entry)));
    new_game (filename, &seed);
  }

	gtk_widget_hide(app);
}

void show_select_game_dialog() 
{
  static GtkWidget* dialog = NULL;
  static GtkWidget* seed_entry;
  static GtkListStore* list;
  static GtkWidget* list_view;
  static GtkTreeSelection* select;
  GtkWidget* scrolled_window;
  GtkTreeViewColumn* column;
  GtkCellRenderer* renderer;
  GtkTreeIter iter;
  GtkTreeIter selected_iter;
  GtkTreePath * path;
  
  guint i;
  gchar buf[20];

  if (waiting_for_mouse_up()) return;

  if(!dialog) {

    GtkWidget* label;
    GtkWidget* hbox;

    dialog = gtk_dialog_new_with_buttons (_("Select Game"),
                                          GTK_WINDOW(app),
                                          GTK_DIALOG_DESTROY_WITH_PARENT,
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OK, GTK_RESPONSE_OK,
                                          NULL);
    gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
    hbox = gtk_hbox_new (FALSE, GNOME_PAD_SMALL);
    seed_entry = gtk_entry_new ();
    label = gtk_label_new(_("Seed"));
    
    gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 
			GNOME_PAD_SMALL );
    gtk_box_pack_end (GTK_BOX (hbox), seed_entry, FALSE, FALSE, 
		      GNOME_PAD_SMALL );
    
    gtk_box_pack_end (GTK_BOX (GTK_DIALOG (dialog)->vbox), 
		      hbox, FALSE, FALSE, GNOME_PAD_SMALL );
    
    hbox = gtk_hbox_new (FALSE, GNOME_PAD_SMALL);

    list = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
    list_view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(list));
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (list_view), FALSE);
    g_object_unref (G_OBJECT (list));
    
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Rules"),
                                                      renderer,
                                                      "text", 0,
                                                      NULL);
    
    gtk_tree_view_append_column(GTK_TREE_VIEW (list_view), column);
    
    select = gtk_tree_view_get_selection(GTK_TREE_VIEW (list_view));
    gtk_tree_selection_set_mode (select, GTK_SELECTION_BROWSE); 

    g_signal_connect (G_OBJECT (select), "changed", 
                      GTK_SIGNAL_FUNC (select_rules), NULL);
		

    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolled_window), GTK_SHADOW_IN);
    gtk_widget_set_size_request (scrolled_window, 300, 250);
    gtk_container_add (GTK_CONTAINER (scrolled_window), list_view);

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				    GTK_POLICY_NEVER,
				    GTK_POLICY_AUTOMATIC);
    
    gtk_box_pack_end (GTK_BOX (hbox), scrolled_window, TRUE, TRUE,
		      GNOME_PAD_SMALL );
  
    gtk_box_pack_end (GTK_BOX (GTK_DIALOG (dialog)->vbox), 
		      hbox, TRUE, TRUE, GNOME_PAD_SMALL );

    filename = NULL;
    selected_iter.stamp = 0;
    
    for(i = 0; i < n_games; i++) {
	    gchar *text;
	    gint row;
	    text = game_file_to_name (game_dents[i]->d_name);
	    gtk_list_store_append (GTK_LIST_STORE (list), &iter);
            gtk_list_store_set(GTK_LIST_STORE (list), 
                               &iter, 0, text, 1,
                               game_dents[i]->d_name, -1);
            if (g_utf8_collate(text,game_name) == 0) {
                    memcpy (&selected_iter, &iter, sizeof(GtkTreeIter));
            }
    }

    gtk_dialog_set_default_response ( GTK_DIALOG (dialog), GTK_RESPONSE_OK );

    g_signal_connect (GTK_OBJECT (dialog), "response", 
			                GTK_SIGNAL_FUNC (select_game), seed_entry);

    
    g_signal_connect (GTK_WIDGET (dialog), "delete_event",
		                  GTK_SIGNAL_FUNC(gtk_widget_hide), NULL);

    gtk_widget_show_all (dialog);

    if (selected_iter.stamp != 0)
      gtk_tree_selection_select_iter (select, &selected_iter);    
  }
  
  g_snprintf (buf, sizeof (buf), "%d", seed);
  gtk_entry_set_text (GTK_ENTRY (seed_entry), buf);

  if (gtk_tree_selection_get_selected (select, (GtkTreeModel **)(&list), &iter)) {
    path = gtk_tree_model_get_path (GTK_TREE_MODEL(list), &iter);
    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (list_view), path, NULL, TRUE, 0.5, 0.0);
    gtk_tree_path_free (path);
  }
  
  gtk_dialog_run (GTK_DIALOG (dialog)); 
}

  static void
hint_destroy_callback (void)
{
  hint_dlg = NULL;
}

void show_hint_dialog() 
{
  char *gmessage;
  char *str1, *str2;

  if (waiting_for_mouse_up()) return;

  if (game_over) {
    gmessage = g_strdup (_("The game is over.\nNo hints are available"));
  }  
  else {
    SCM hint = gh_call0(game_data->hint_lambda);

    if (!gh_scm2bool(hint)) {
      gmessage = g_strdup (_("This game does not have hint support yet."));
    }
    else {
      switch (gh_scm2int(gh_car(hint))) {

      case 0:
	gmessage = g_strdup (gh_scm2newstr(gh_cadr(hint),NULL));
	break;

      case 1:
	str1 = gh_scm2newstr(gh_cadr(hint),NULL);
	str2 = gh_scm2newstr(gh_caddr(hint),NULL);
	gmessage = g_strdup_printf (_("Move %s onto %s."), str1, str2);
	free (str1);
	free (str2);
	break;

      case 2:
	str1 = gh_scm2newstr(gh_cadr(hint),NULL);
	str2 = gh_scm2newstr(gh_caddr(hint),NULL);
	gmessage = g_strdup_printf (_("Move %s onto %s."), str1, str2);
	free (str1);
	free (str2);
	break;

      case 3: /* This is deprecated (due to i18n issues) do not use.*/
	str1 = gh_scm2newstr(gh_cadr(hint),NULL);
	str2 = gh_scm2newstr(gh_caddr(hint),NULL);
	gmessage = g_strdup_printf (_("Move %s %s."), str1, str2);
	free (str1);
	free (str2);
        g_warning(_("This game uses a deprecated hint method (case 3).\n Please file a bug at http://bugzilla.gnome.org including this message and\n the name of the game you where playing (look in the title bar if you \naren't sure)."));
	break;

      case 4:
	str1 = gh_scm2newstr(gh_cadr(hint),NULL);
	gmessage = g_strdup_printf (_("You are searching for a %s."), str1);
	free (str1);
	break;

      default:
	gmessage = g_strdup (_("This game is unable to provide a hint."));
	break;
      }
    }
	}

  if (hint_dlg) {
   gtk_widget_destroy (GTK_WIDGET (hint_dlg));
	  
  }

  hint_dlg = gtk_message_dialog_new (GTK_WINDOW (app),
	                                   GTK_DIALOG_DESTROY_WITH_PARENT,
	                                   GTK_MESSAGE_INFO,
	                                   GTK_BUTTONS_OK,
	                                   gmessage);
  gtk_dialog_set_has_separator (GTK_DIALOG (hint_dlg), FALSE);
  if (hint_dlg) {
	  g_signal_connect (GTK_OBJECT (hint_dlg),
			      "destroy",
			      (GtkSignalFunc) hint_destroy_callback,
			      NULL);
  }

	gtk_dialog_run(GTK_DIALOG(hint_dlg));
	gtk_widget_destroy(hint_dlg);

  g_free (gmessage);
}

GtkWidget *
get_main_page (GtkWidget* dialog)
{
  GtkWidget *retval = gtk_vbox_new (FALSE, GNOME_PAD_SMALL);

  gtk_widget_show_all (retval);
  return retval;
}

GtkWidget *
get_background_page (GtkWidget* dialog)
{
  GtkWidget *retval = gtk_vbox_new (FALSE, GNOME_PAD_SMALL);

  gtk_widget_show_all (retval);
  return retval;
}

static GtkWidget * deck_edit = NULL;

static void 
property_apply (GtkWidget *w, gpointer data)
{
  gtk_object_destroy (card_deck);
  deck_options =  
    gtk_card_deck_options_edit_get (GTK_CARD_DECK_OPTIONS_EDIT (w));
  card_deck = gdk_card_deck_new (app->window, deck_options);
  gconf_client_set_string (gconf_client, "/apps/aisleriot/card_options",
                           deck_options, NULL);

  refresh_screen();
}

void show_preferences_dialog () 
{
  static GtkWidget* property_box = NULL;
  GtkWidget * frame;
  gchar * frame_title;
  
  if (!property_box) {
    property_box = gtk_dialog_new ();
    gtk_dialog_set_has_separator (GTK_DIALOG (property_box), FALSE);
    gtk_window_set_title (GTK_WINDOW (property_box), _("AisleRiot Preferences"));
    gtk_dialog_add_buttons(GTK_DIALOG(property_box),
                           GTK_STOCK_CLOSE, GTK_RESPONSE_OK, NULL);
    gtk_window_set_transient_for (GTK_WINDOW(property_box), GTK_WINDOW (app));

    frame_title = g_strdup_printf ("<b>%s</b>",_("Card Style"));
    frame = gtk_frame_new (frame_title);
    g_free (frame_title);
    gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);
    gtk_label_set_use_markup (GTK_LABEL (gtk_frame_get_label_widget(GTK_FRAME(frame))), TRUE);
    gtk_misc_set_alignment (GTK_MISC (gtk_frame_get_label_widget(GTK_FRAME(frame))), 0, 0.5);
    
    deck_edit = gtk_card_deck_options_edit_new ();
    gtk_container_add (GTK_CONTAINER (frame), deck_edit);
    
    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (property_box)->vbox), frame,
                        TRUE, TRUE, GNOME_PAD_SMALL);
    
    g_signal_connect (G_OBJECT (deck_edit), "changed",
                      GTK_SIGNAL_FUNC (property_apply), NULL);
    
    g_signal_connect (G_OBJECT (property_box), "response",
                      GTK_SIGNAL_FUNC (gtk_widget_hide), NULL);

    g_signal_connect(G_OBJECT (property_box), "delete_event",
                     GTK_SIGNAL_FUNC(gtk_widget_hide), NULL);

    gtk_widget_show_all (property_box);
  }
  gtk_card_deck_options_edit_set (GTK_CARD_DECK_OPTIONS_EDIT (deck_edit),
                                  deck_options);
  gtk_window_present (GTK_WINDOW (property_box));
}

SCM options = SCM_BOOL_F;
GtkWidget *option_page;

static void 
option_apply (GtkWidget *w, int response)
{
    SCM opts = options;
    GList *item = GTK_BOX (option_page)->children;
    
		if ( response != GTK_RESPONSE_OK )
			return;
		
    for(; opts != SCM_EOL; item = item->next, opts = gh_cdr (opts)) {
      
      SCM_SETCAR (gh_cdar(opts), 
		gh_bool2scm (GTK_TOGGLE_BUTTON 
			     (((GtkBoxChild *) item->data)->widget)->active));
    }    
    gh_call1(game_data->apply_options_lambda, options);
}

GtkWidget *
get_option_page (GtkWidget* option_dialog)
{
  SCM opts;
  GtkWidget* vbox = gtk_vbox_new(FALSE, GNOME_PAD_SMALL);

  for(opts = options; opts != SCM_EOL; opts = gh_cdr (opts)) {
    GtkWidget *toggle = 
      gtk_check_button_new_with_label (_(gh_scm2newstr(gh_caar(opts), 
						       NULL)));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle), 
				 gh_scm2bool(gh_cadar(opts)));
    gtk_box_pack_end (GTK_BOX (vbox), toggle, 
		      FALSE, FALSE, GNOME_PAD_SMALL);
  }
  return vbox;
}

void show_rules_options_dialog () 
{
	static GtkWidget *notebook = NULL;
  /* Need to call this every time as it might be garbage collected ! */
  options = gh_call0(game_data->get_options_lambda);
  
  if (!option_dialog && gh_scm2bool(options)) {
    option_dialog = gtk_dialog_new ();
    gtk_dialog_set_has_separator (GTK_DIALOG (option_dialog), FALSE);
		notebook = gtk_notebook_new();

		gtk_container_add (GTK_CONTAINER(GTK_DIALOG(option_dialog)->vbox),
		                   notebook);
		
		gtk_dialog_add_buttons(GTK_DIALOG(option_dialog),
		                       GTK_STOCK_OK,
													 GTK_RESPONSE_OK,
													 GTK_STOCK_CANCEL,
													 GTK_RESPONSE_CANCEL);

    gtk_window_set_transient_for (GTK_WINDOW (option_dialog), GTK_WINDOW (app));
		gtk_window_set_modal(GTK_WINDOW(option_dialog), TRUE);
 
    option_page = get_option_page(option_dialog);

		gtk_notebook_append_page (GTK_NOTEBOOK (notebook), 
				      option_page, 
				      gtk_label_new(game_name));

    g_signal_connect (GTK_OBJECT (option_dialog), "response",
			GTK_SIGNAL_FUNC (option_apply), NULL);

    g_signal_connect (GTK_OBJECT (option_dialog), "delete_event",
			GTK_SIGNAL_FUNC (gtk_widget_hide), NULL);
  } 
  gtk_window_present (GTK_WINDOW (option_dialog));
}

void show_global_stats_dialog () 
{
}

void show_rules_stats_dialog () 
{
}
