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

#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <gnome.h>
#include <games-frame.h>
#include <games-card-selector.h>
#include "cscmi.h"
#include "sol.h"
#include "menu.h"
#include "dialog.h"
#include "draw.h"
#include "events.h"

static GtkWidget *hint_dlg = NULL;
static GtkTreeIter selected_iter;

void show_game_over_dialog() {
  GtkWidget* dialog;
  gchar* message;

  if (game_won)
    message = g_strdup_printf ("<b>%s\n\n%s</b>", _("Congratulations."),
                                _("You have won!!!"));
  else
    message = g_strdup_printf ("<b>%s</b>", _("There are no more moves."));

  dialog = gtk_message_dialog_new_with_markup (GTK_WINDOW (app),
					       GTK_DIALOG_DESTROY_WITH_PARENT,
					       GTK_MESSAGE_INFO,
					       GTK_BUTTONS_NONE,
					       message);

  g_free (message);

  if (!game_won)
    gtk_dialog_add_buttons (GTK_DIALOG (dialog),
			    GTK_STOCK_UNDO, GTK_RESPONSE_REJECT,
			    NULL);
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
			  GTK_STOCK_QUIT, GTK_RESPONSE_CLOSE,
                          _("New Game"), GTK_RESPONSE_ACCEPT,
                          NULL);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  /* add a stock icon? */ 
  switch (gtk_dialog_run (GTK_DIALOG (dialog)))
    {
    case GTK_RESPONSE_ACCEPT: 
      gtk_widget_destroy (dialog);
      new_game (NULL, NULL);
      break;
    case GTK_RESPONSE_REJECT:
      timer_restart ();
      gtk_widget_destroy (dialog);
      game_over = FALSE;
      undo_callback ();
      break;
    case GTK_RESPONSE_CLOSE:
      gtk_main_quit ();
      break;
    default:
      /* The player closed the dialog window, just ignore this. */
      game_over = FALSE;
      gtk_widget_destroy (dialog);
  }
}

gchar *filename = NULL;

static void select_rules(GtkTreeSelection *select, gpointer data)
{
	GtkTreeModel *model;
        GtkTreeIter iter;

	if (gtk_tree_selection_get_selected(select, &model, &iter)) {
	     g_free (filename);
	     gtk_tree_model_get(model, &iter, 1, &filename, -1);
	} else {
	     g_free (filename);
	     filename = NULL;
	}
}

static void select_game (GtkWidget *app, gint response, gpointer data)
{
  if(response == GTK_RESPONSE_OK) {
       if (filename == NULL)
	    return;

       new_game (filename, NULL);
  }

  gtk_widget_hide(app);
}

static void
select_double_click (GtkWidget *widget, GtkTreePath *path,
                     GtkTreeViewColumn *column, gpointer dialog)
{
  /* Handle a double click by faking a click on the OK button. */
  select_game (GTK_WIDGET (dialog), GTK_RESPONSE_OK, NULL);
}

static void build_list (gchar * filename, GtkWidget * list)
{
  gchar *text;
  GtkTreeIter iter;

  if (g_utf8_collate (filename, "sol.scm") == 0)
    return;

  text = game_file_to_name (filename);
  gtk_list_store_append (GTK_LIST_STORE (list), &iter);
  gtk_list_store_set(GTK_LIST_STORE (list), 
                     &iter, 0, text, 1,
                     filename, -1);
  if (g_utf8_collate(text,game_name) == 0) {
    memcpy (&selected_iter, &iter, sizeof(GtkTreeIter));
  }
  g_free (text);
}

void show_select_game_dialog (void) 
{
  static GtkWidget* dialog = NULL;
  static GtkListStore* list;
  static GtkWidget* list_view;
  static GtkTreeSelection* select;
  GtkWidget* scrolled_window;
  GtkTreeViewColumn* column;
  GtkCellRenderer* renderer;
  GtkTreeIter iter;
  GtkTreePath * path;
  GamesFileList * files;

  if (waiting_for_mouse_up()) return;

  if (!dialog) {

    GtkWidget* hbox;

    dialog = gtk_dialog_new_with_buttons (_("Select Game"),
                                          GTK_WINDOW(app),
                                          GTK_DIALOG_DESTROY_WITH_PARENT,
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OK, GTK_RESPONSE_OK,
                                          NULL);
    gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
    gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
    gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (dialog)->vbox), 2);
    hbox = gtk_hbox_new (FALSE, 12);
    gtk_container_set_border_width (GTK_CONTAINER (hbox), 5);

    list = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
    list_view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(list));
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (list_view), FALSE);
    g_object_unref (G_OBJECT (list));
    
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Rules"),
                                                      renderer,
                                                      "text", 0,
                                                      NULL);
    
    gtk_tree_view_append_column (GTK_TREE_VIEW (list_view), column);
    
    select = gtk_tree_view_get_selection(GTK_TREE_VIEW (list_view));
    gtk_tree_selection_set_mode (select, GTK_SELECTION_BROWSE); 

    g_signal_connect (G_OBJECT (select), "changed", 
                      G_CALLBACK (select_rules), NULL);
    g_signal_connect (G_OBJECT (list_view), "row-activated",
		      G_CALLBACK (select_double_click), dialog);

    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolled_window), GTK_SHADOW_IN);
    gtk_widget_set_size_request (scrolled_window, 300, 250);
    gtk_container_add (GTK_CONTAINER (scrolled_window), list_view);

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				    GTK_POLICY_NEVER,
				    GTK_POLICY_AUTOMATIC);
    
    gtk_box_pack_end (GTK_BOX (hbox), scrolled_window, TRUE, TRUE,
		      0 );
  
    gtk_box_pack_end (GTK_BOX (GTK_DIALOG (dialog)->vbox), 
		      hbox, TRUE, TRUE, 0 );

    filename = NULL;
    selected_iter.stamp = 0;
    
    files = games_file_list_new ("*.scm", gamesdir, NULL);
    games_file_list_transform_basename (files);
 
    games_file_list_for_each (files, (GFunc) build_list, list);

    g_free (files);

    gtk_dialog_set_default_response ( GTK_DIALOG (dialog), GTK_RESPONSE_OK );

    g_signal_connect (G_OBJECT (dialog), "response", 
                      G_CALLBACK (select_game), NULL);

    
    g_signal_connect (G_OBJECT (dialog), "delete_event",
                      G_CALLBACK(gtk_widget_hide), NULL);

    gtk_widget_show_all (dialog);

    if (selected_iter.stamp != 0)
      gtk_tree_selection_select_iter (select, &selected_iter);    
  }
  
  if (gtk_tree_selection_get_selected (select, NULL, &iter)) {
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
    SCM hint = cscmi_hint_lambda ();

    if (!SCM_NFALSEP (hint)) {
      gmessage = g_strdup (_("This game does not have hint support yet."));
    }
    else {
      switch (scm_num2int (SCM_CAR (hint), SCM_ARG1, NULL)) {

      case 0:
	gmessage = g_strdup (SCM_STRING_CHARS (SCM_CADR(hint)));
	break;

      case 1:
	str1 = SCM_STRING_CHARS (SCM_CADR (hint));
	str2 = SCM_STRING_CHARS (SCM_CADDR (hint));
	gmessage = g_strdup_printf (_("Move %s onto %s."), str1, str2);
	break;

      case 2:
	str1 = SCM_STRING_CHARS (SCM_CADR (hint));
	str2 = SCM_STRING_CHARS (SCM_CADDR (hint));
	gmessage = g_strdup_printf (_("Move %s onto %s."), str1, str2);
	break;

      case 3: /* This is deprecated (due to i18n issues) do not use.*/
	str1 = SCM_STRING_CHARS (SCM_CADR (hint));
	str2 = SCM_STRING_CHARS (SCM_CADDR (hint));
	gmessage = g_strdup_printf (_("Move %s %s."), str1, str2);
        g_warning(_("This game uses a deprecated hint method (case 3).\nPlease file a bug at http://bugzilla.gnome.org including this message and\nthe name of the game you where playing (look in the title bar if you \naren't sure)."));
	break;

      case 4:
	str1 = SCM_STRING_CHARS (SCM_CADR (hint));
	gmessage = g_strdup_printf (_("You are searching for a %s."), str1);
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

  hint_dlg = gtk_message_dialog_new_with_markup (GTK_WINDOW (app),
				     GTK_DIALOG_DESTROY_WITH_PARENT,
				     GTK_MESSAGE_INFO,
				     GTK_BUTTONS_OK,
				     "<b>%s</b>", gmessage);
  gtk_window_set_title (GTK_WINDOW (hint_dlg), "");

  if (hint_dlg) {
	  g_signal_connect (GTK_OBJECT (hint_dlg),
			      "destroy",
			      (GtkSignalFunc) hint_destroy_callback,
			      NULL);
  }

  gtk_dialog_run (GTK_DIALOG (hint_dlg));
  gtk_widget_destroy (hint_dlg);

  g_free (gmessage);
}

static GtkWidget * deck_edit = NULL;

static void 
property_apply (GtkWidget *w, gchar * name, gpointer data)
{
  gconf_client_set_string (gconf_client, THEME_GCONF_KEY, name, NULL);
  /* Inconvenient fact of the day: the string "name" is in fact owned by 
   * something that could go away. We have conveniently arranged so that it
   * doesn't, but card_style should probably be removed as a variable. */
  card_style = name;

  set_card_theme (name);
  
  refresh_screen();
}

void show_preferences_dialog () 
{
  static GtkWidget* property_box = NULL;
  
  if (!property_box) {
    property_box = gtk_dialog_new ();
    gtk_dialog_set_has_separator (GTK_DIALOG (property_box), FALSE);
    gtk_widget_set_size_request (property_box, 220, -1);
    gtk_window_set_resizable (GTK_WINDOW (property_box), FALSE);
    gtk_container_set_border_width (GTK_CONTAINER (property_box), 5);
    gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (property_box)->vbox), 2);
    gtk_window_set_title (GTK_WINDOW (property_box), _("AisleRiot Cards"));
    gtk_dialog_add_buttons(GTK_DIALOG(property_box),
                           GTK_STOCK_CLOSE, GTK_RESPONSE_OK, NULL);
    gtk_window_set_transient_for (GTK_WINDOW(property_box), GTK_WINDOW (app));
     
    deck_edit = games_card_selector_new (card_style);
    gtk_container_set_border_width (GTK_CONTAINER (deck_edit), 5);
    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (property_box)->vbox), deck_edit,
                        TRUE, TRUE, 0);
    
    g_signal_connect (G_OBJECT (deck_edit), "changed",
                      GTK_SIGNAL_FUNC (property_apply), NULL);
    
    g_signal_connect (G_OBJECT (property_box), "response",
                      GTK_SIGNAL_FUNC (gtk_widget_hide), NULL);

    g_signal_connect(G_OBJECT (property_box), "delete_event",
                     GTK_SIGNAL_FUNC(gtk_widget_hide), NULL);

    gtk_widget_show_all (property_box);
  }

  gtk_window_present (GTK_WINDOW (property_box));
}

#if 0

SCM options = SCM_BOOL_F;
GtkWidget *option_page;

static void 
option_apply (GtkWidget *w, int response)
{
    SCM opts = options;
    GList *item = GTK_BOX (option_page)->children;
    
		if ( response != GTK_RESPONSE_OK )
			return;
		
    for(; opts != SCM_EOL; item = item->next, opts = SCM_CDR (opts)) {
      
      SCM_SETCAR (SCM_CDAR(opts), 
		  SCM_BOOL (GTK_TOGGLE_BUTTON 
			    (((GtkBoxChild *) item->data)->widget)->active));
    }    
    cscmi_apply_options_lambda (options);
}

static GtkWidget *
get_option_page (GtkWidget* option_dialog)
{
  SCM opts;
  GtkWidget* vbox = gtk_vbox_new(FALSE, GNOME_PAD_SMALL);

  for(opts = options; opts != SCM_EOL; opts = SCM_CDR (opts)) {
    GtkWidget *toggle = 
      gtk_check_button_new_with_label (_(SCM_STRING_CHARS (SCM_CAAR(opts))));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle), 
				 SCM_NFALSEP(SCM_CADAR(opts)));
    gtk_box_pack_end (GTK_BOX (vbox), toggle, 
		      FALSE, FALSE, GNOME_PAD_SMALL);
  }
  return vbox;
}

void show_rules_options_dialog () 
{
	static GtkWidget *notebook = NULL;
  /* Need to call this every time as it might be garbage collected ! */
	options = cscmi_get_options_lambda ();
  
  if (!option_dialog && SCM_NFALSEP(options)) {
    option_dialog = gtk_dialog_new ();
    gtk_dialog_set_has_separator (GTK_DIALOG (option_dialog), FALSE);
		notebook = gtk_notebook_new();

		gtk_container_add (GTK_CONTAINER(GTK_DIALOG(option_dialog)->vbox),
		                   notebook);
		
		gtk_dialog_add_buttons(GTK_DIALOG(option_dialog),
		                       GTK_STOCK_OK,
				       GTK_RESPONSE_OK,
				       GTK_STOCK_CANCEL,
				       GTK_RESPONSE_CANCEL, NULL);

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

#endif

void show_global_stats_dialog () 
{
}

void show_rules_stats_dialog () 
{
}
