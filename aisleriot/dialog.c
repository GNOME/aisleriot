#include <stdlib.h>
#include <guile/gh.h>
#include <dirent.h>
#include "gnome.h"
#include "cscmi.h"
#include "sol.h"
#include "menu.h"
#include "dialog.h"
#include "draw.h"

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
  gnome_dialog_set_default ( GNOME_DIALOG (dialog), 0 );
  if (gnome_dialog_run (GNOME_DIALOG (dialog)) == 0) 
    new_game (NULL, NULL);
#else
  gnome_app_question_modal ( GNOME_APP (app), message, random_seed, NULL);
#endif
}

gchar *filename;

void select_rules (GtkWidget* menu_item, gchar* file)
{
  filename = file;
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
  static GtkWidget* option_menu;
  guint i;
  gchar buf[20];

  if(!dialog) {

    GtkWidget* menu;
    GtkWidget* menu_item;
    GtkWidget* label;
    GtkWidget* hbox;
    gchar* message = _("Select Game");

    dialog = gnome_message_box_new (message, GNOME_MESSAGE_BOX_QUESTION,
				    GNOME_STOCK_BUTTON_OK, 
				    GNOME_STOCK_BUTTON_CANCEL,
				    NULL );
    gnome_dialog_set_default ( GNOME_DIALOG (dialog), 0 );

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
    option_menu = gtk_option_menu_new ();
    label = gtk_label_new(_("Rules"));
    
    gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 
			GNOME_PAD_SMALL );
    gtk_box_pack_end (GTK_BOX (hbox), option_menu, FALSE, FALSE, 
		      GNOME_PAD_SMALL );
    
    gtk_box_pack_end (GTK_BOX (GNOME_DIALOG (dialog)->vbox), 
		      hbox, FALSE, FALSE, GNOME_PAD_SMALL );
        
    filename = NULL;
    menu = gtk_menu_new();
    
    for(i = 0; i < n_games; i++) {
      menu_item = gtk_menu_item_new_with_label 
	(game_file_to_name (game_dents[i]->d_name));
      gtk_signal_connect (GTK_OBJECT(menu_item), "activate", 
			  (GtkSignalFunc) select_rules,
			  (gpointer) game_dents[i]->d_name);
      gtk_menu_shell_append (GTK_MENU_SHELL(menu), menu_item);
    }
    gtk_widget_show_all (menu);
    
    gtk_option_menu_set_menu (GTK_OPTION_MENU (option_menu), menu);

    gnome_dialog_editable_enters (GNOME_DIALOG (dialog), 
				  GTK_EDITABLE (seed_entry));
    
    gtk_signal_connect (GTK_OBJECT (dialog), "clicked", 
			GTK_SIGNAL_FUNC (select_game), seed_entry);

    gtk_widget_show_all (dialog);
    
    gnome_dialog_close_hides (GNOME_DIALOG (dialog), TRUE);
  }

  /* Can we respect user prefs for status bar using libgnomeui ???
   * Current app-utils look insufficient to me. */
  for(i = 0; i < n_games; i++)
    if (!strcmp (game_dents[i]->d_name, game_file))
      gtk_option_menu_set_history (GTK_OPTION_MENU (option_menu), i);
  
  sprintf (buf, "%d", seed);
  gtk_entry_set_text (GTK_ENTRY (seed_entry), buf);

  gnome_dialog_run (GNOME_DIALOG (dialog));
}

void show_hint_dialog() 
{
  GString* gmessage;

  if (game_over) {
    gmessage = g_string_new (_("The game is over.\nNo hints are available"));
  }  
  else {
    SCM hint = gh_call0(game_data->hint_lambda);

    if (!gh_scm2bool(hint)) {
      gmessage = g_string_new (_("This game does not have hint support yet."));
    }
    else {
      switch (gh_scm2int(gh_car(hint))) {

      case 0:
	gmessage = g_string_new (gh_scm2newstr(gh_cadr(hint),NULL));
	break;

      case 1:
	gmessage = g_string_new (_("Move the "));
	g_string_append (gmessage, gh_scm2newstr(gh_cadr(hint),NULL));
	g_string_append (gmessage, _(" on the "));
	g_string_append (gmessage, gh_scm2newstr(gh_caddr(hint),NULL));
	g_string_append (gmessage, _("."));
	break;

      case 2:
	gmessage = g_string_new (_("Move the "));
	g_string_append (gmessage, gh_scm2newstr(gh_cadr(hint),NULL));
	g_string_append (gmessage, _(" on "));
	g_string_append (gmessage, gh_scm2newstr(gh_caddr(hint),NULL));
	g_string_append (gmessage, _("."));
	break;

      case 3:
	gmessage = g_string_new (_("Move the "));
	g_string_append (gmessage, gh_scm2newstr(gh_cadr(hint),NULL));
	g_string_append (gmessage, _(" "));
	g_string_append (gmessage, gh_scm2newstr(gh_caddr(hint),NULL));
	g_string_append (gmessage, _("."));
	break;

      case 4:
	gmessage = g_string_new (_("You are searching for a "));
	g_string_append (gmessage, gh_scm2newstr(gh_cadr(hint),NULL));
	g_string_append (gmessage, _("."));
	break;

      default:
	gmessage = g_string_new (_("This game is unable to provide a hint."));
	break;
      }
    }
  }

  /* Respects user prefs on status bar for hints: */
  gnome_app_message (GNOME_APP (app), gmessage->str);
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

void show_property_dialog () 
{
  static GtkWidget* property_box = NULL;

  if (!property_box) {
    property_box = gnome_property_box_new ();
  
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
    gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (toggle), 
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
