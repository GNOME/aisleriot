/* AisleRiot - dialog.c
 * Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
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

#include <stdlib.h>
#include <guile/gh.h>
#include <dirent.h>
#include "config.h"
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
    message = _("Congratulations\n\nYou Won!!!");
  else
    message = _("\nGame Over.\n");

  /* Should we use the status bar for game over reports when the user
   * look & feel prefs go for the status bar ? I think not... */
#if 1
  dialog = gnome_message_box_new (message, GNOME_MESSAGE_BOX_QUESTION,
				  _("New Game"), GNOME_STOCK_BUTTON_CANCEL, 
				  NULL);
  gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (app));
  gnome_dialog_set_default ( GNOME_DIALOG (dialog), 0 );
  if (gnome_dialog_run (GNOME_DIALOG (dialog)) == 0) 
    new_game (NULL, NULL);
#else
  gnome_app_question_modal ( GNOME_APP (app), message, random_seed, NULL);
#endif
}

gchar *filename = NULL;

void select_rules(GtkCList       *clist,
		  gint            row,
		  gint            column,
		  GdkEvent       *event)
{
	if (row == -1)
		return;

	filename = gtk_clist_get_row_data (GTK_CLIST (clist), row);
}

void select_game (GtkWidget *app, gint button, GtkWidget* entry)
{
  if(button == 0) {
    seed = atoi (gtk_entry_get_text (GTK_ENTRY (entry)));
    new_game (filename, &seed);
  }
}

void show_select_game_dialog() 
{
  static GtkWidget* dialog = NULL;
  static GtkWidget* seed_entry;
  static GtkWidget* list;
  GtkWidget* scrolled_window;
  
  guint i;
  gchar buf[20];

  if (waiting_for_mouse_up()) return;

  if(!dialog) {

    GtkWidget* label;
    GtkWidget* hbox;
    gchar* message = _("Select Game");

    dialog = gnome_message_box_new (message, GNOME_MESSAGE_BOX_QUESTION,
				    GNOME_STOCK_BUTTON_OK, 
				    GNOME_STOCK_BUTTON_CANCEL,
				    NULL );
    gtk_window_set_policy (GTK_WINDOW (dialog), FALSE, TRUE, FALSE);
    gnome_dialog_set_default ( GNOME_DIALOG (dialog), 0 );

    gnome_dialog_set_parent ( GNOME_DIALOG (dialog), GTK_WINDOW (app) );

    hbox = gtk_hbox_new (FALSE, GNOME_PAD_SMALL);
    seed_entry = gtk_entry_new ();
    label = gtk_label_new(_("Seed"));
    
    gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 
			GNOME_PAD_SMALL );
    gtk_box_pack_end (GTK_BOX (hbox), seed_entry, FALSE, FALSE, 
		      GNOME_PAD_SMALL );
    
    gtk_box_pack_end (GTK_BOX (GNOME_DIALOG (dialog)->vbox), 
		      hbox, FALSE, FALSE, GNOME_PAD_SMALL );
    
    hbox = gtk_hbox_new (FALSE, GNOME_PAD_SMALL);

    list = gtk_clist_new (1);
    gtk_signal_connect (GTK_OBJECT (list), "select_row", select_rules, NULL);
    gtk_clist_set_selection_mode (GTK_CLIST (list), GTK_SELECTION_BROWSE);
    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_widget_set_usize (scrolled_window, 300, 250);
    gtk_container_add (GTK_CONTAINER (scrolled_window), list);

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				    GTK_POLICY_NEVER,
				    GTK_POLICY_AUTOMATIC);
				    
    
    label = gtk_label_new(_("Rules"));
    
    gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 
			GNOME_PAD_SMALL );
    gtk_box_pack_end (GTK_BOX (hbox), scrolled_window, TRUE, TRUE,
		      GNOME_PAD_SMALL );
  
    gtk_box_pack_end (GTK_BOX (GNOME_DIALOG (dialog)->vbox), 
		      hbox, FALSE, FALSE, GNOME_PAD_SMALL );
        
    filename = NULL;
    
    for(i = 0; i < n_games; i++) {
	    gchar *text[1];
	    gint row;
	    text[0] = game_file_to_name (game_dents[i]->d_name);
	    row = gtk_clist_append (GTK_CLIST (list), text);
	    gtk_clist_set_row_data (GTK_CLIST (list), row, game_dents[i]->d_name);
    }

    
    gnome_dialog_editable_enters (GNOME_DIALOG (dialog), 
				  GTK_EDITABLE (seed_entry));
    
    gtk_signal_connect (GTK_OBJECT (dialog), "clicked", 
			GTK_SIGNAL_FUNC (select_game), seed_entry);

    gtk_widget_show_all (dialog);
    
    gnome_dialog_close_hides (GNOME_DIALOG (dialog), TRUE);
  }

  /* Can we respect user prefs for status bar using libgnomeui ???
   * Current app-utils look insufficient to me. */
  for(i = 0; i < n_games; i++) {
	  gchar *text = NULL;
	  text = (gchar *) gtk_clist_get_row_data (GTK_CLIST (list), i);
	  if (!strcmp (text, game_file)) {
		  gtk_clist_select_row (GTK_CLIST (list), i, 0);
		  break;
	  }
  }

  g_snprintf (buf, sizeof (buf), "%d", seed);
  gtk_entry_set_text (GTK_ENTRY (seed_entry), buf);

  gnome_dialog_run (GNOME_DIALOG (dialog));
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
        /* This is discouraged, as it makes I18N a nightmare */
	gmessage = g_strdup (_(gh_scm2newstr(gh_cadr(hint),NULL)));
	break;

      case 1:
	str1 = gh_scm2newstr(gh_cadr(hint),NULL);
	str2 = gh_scm2newstr(gh_caddr(hint),NULL);
	gmessage = g_strdup_printf (_("Move the %s on the %s."), str1, str2);
	free (str1);
	free (str2);
	break;

      case 2:
	str1 = gh_scm2newstr(gh_cadr(hint),NULL);
	str2 = gh_scm2newstr(gh_caddr(hint),NULL);
	gmessage = g_strdup_printf (_("Move the %s on %s."), str1, str2);
	free (str1);
	free (str2);
	break;

      case 3:
	str1 = gh_scm2newstr(gh_cadr(hint),NULL);
	str2 = gh_scm2newstr(gh_caddr(hint),NULL);
	gmessage = g_strdup_printf (_("Move the %s %s."), str1, str2);
	free (str1);
	free (str2);
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
   gnome_dialog_close (GNOME_DIALOG (hint_dlg));
	  
  }

  hint_dlg = gnome_ok_dialog_parented (gmessage, GTK_WINDOW (app));
  if (hint_dlg) {
	  gtk_signal_connect (GTK_OBJECT (hint_dlg),
			      "destroy",
			      (GtkSignalFunc) hint_destroy_callback,
			      NULL);
  }

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

static GtkObject* deck_edit = NULL;

static void 
property_apply (GtkWidget *w, int page)
{
  if (gdk_card_deck_options_edit_dirty 
      (GDK_CARD_DECK_OPTIONS_EDIT (deck_edit))) {
    gtk_object_destroy (card_deck);
    deck_options =  
      gdk_card_deck_options_edit_get (GDK_CARD_DECK_OPTIONS_EDIT (deck_edit));
    card_deck = gdk_card_deck_new (app->window, deck_options);
    gnome_config_set_string ("Deck/Options", deck_options);
    refresh_screen();
  }
}

void show_preferences_dialog () 
{
  static GtkWidget* property_box = NULL;

  if (!property_box) {
    property_box = gnome_property_box_new ();

    gnome_dialog_set_parent (GNOME_DIALOG (property_box), GTK_WINDOW (app));
  
    deck_edit = gdk_card_deck_options_edit_new 
      (GTK_NOTEBOOK (GNOME_PROPERTY_BOX (property_box)->notebook));

    gtk_signal_connect_object (GTK_OBJECT (deck_edit), "changed",
			       GTK_SIGNAL_FUNC (gnome_property_box_changed), 
			       GTK_OBJECT (property_box));

    gtk_signal_connect (GTK_OBJECT (property_box), "apply",
			GTK_SIGNAL_FUNC (property_apply), NULL);
    gnome_dialog_close_hides (GNOME_DIALOG (property_box), TRUE);
  }
  if (property_box && !GTK_WIDGET_VISIBLE (property_box)) {
    gdk_card_deck_options_edit_set (GDK_CARD_DECK_OPTIONS_EDIT (deck_edit),
				    deck_options);
    gtk_widget_show_all(property_box);
  }
}

SCM options = SCM_BOOL_F;
GtkWidget *option_page;
gint option_page_num;

static void 
option_apply (GtkWidget *w, int page)
{
  if (page == option_page_num) {
    SCM opts = options;
    GList *item = GTK_BOX (option_page)->children;
    
    for(; opts != SCM_EOL; item = item->next, opts = gh_cdr (opts)) {
      
      SCM_SETCAR (gh_cdar(opts), 
		gh_bool2scm (GTK_TOGGLE_BUTTON 
			     (((GtkBoxChild *) item->data)->widget)->active));
    }    
    gh_call1(game_data->apply_options_lambda, options);
  }
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
    gtk_signal_connect_object (GTK_OBJECT (toggle), "toggled",
			       GTK_SIGNAL_FUNC (gnome_property_box_changed), 
			       GTK_OBJECT (option_dialog));
    gtk_box_pack_end (GTK_BOX (vbox), toggle, 
		      FALSE, FALSE, GNOME_PAD_SMALL);
  }
  return vbox;
}

void show_rules_options_dialog () 
{
  /* Need to call this every time as it might be garbage collected ! */
  options = gh_call0(game_data->get_options_lambda);
  
  if (!option_dialog && gh_scm2bool(options)) {
    option_dialog = gnome_property_box_new ();

    gnome_dialog_set_parent (GNOME_DIALOG (option_dialog), GTK_WINDOW (app));
 
    option_page = get_option_page(option_dialog);

    option_page_num = 
      gnome_property_box_append_page (GNOME_PROPERTY_BOX (option_dialog), 
				      option_page, 
				      gtk_label_new(game_name));

    gtk_signal_connect (GTK_OBJECT (option_dialog), "apply",
			GTK_SIGNAL_FUNC (option_apply), NULL);

    gnome_dialog_close_hides (GNOME_DIALOG (option_dialog), TRUE);
  }
  if (option_dialog && !GTK_WIDGET_VISIBLE (option_dialog)) {
    gtk_widget_show_all(option_dialog);
  }
}

void show_global_stats_dialog () 
{
}

void show_rules_stats_dialog () 
{
}
