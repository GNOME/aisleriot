#include <stdlib.h>
#include <guile/gh.h>
#include <dirent.h>
#include "gnome.h"
#include "cscmi.h"
#include "sol.h"
#include "menu.h"
#include "dialog.h"

void show_game_over_dialog() {
  GtkWidget* dialog;
  gchar* message;

  if (game_won)
    message = _("Congratulations\n\nYou Won!!!");
  else
    message = _("\nGame Over.\n");

#if 1
  dialog = gnome_message_box_new (message, GNOME_MESSAGE_BOX_QUESTION,
				  _("New Game"), GNOME_STOCK_BUTTON_CANCEL, 
				  NULL);
  gnome_dialog_set_default ( GNOME_DIALOG (dialog), 0 );
  if (gnome_dialog_run_modal (GNOME_DIALOG (dialog)) == 0) 
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

    hbox = gtk_hbox_new(0,FALSE);
    seed_entry = gtk_entry_new ();
    label = gtk_label_new(_("Seed"));
    
    gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 
			GNOME_PAD_SMALL );
    gtk_box_pack_end (GTK_BOX (hbox), seed_entry, FALSE, FALSE, 
		      GNOME_PAD_SMALL );
    
    gtk_box_pack_end (GTK_BOX (GNOME_DIALOG (dialog)->vbox), 
		      hbox, FALSE, FALSE, GNOME_PAD_SMALL );
    
    hbox = gtk_hbox_new(0,FALSE);
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

  for(i = 0; i < n_games; i++)
    if (!strcmp (game_dents[i]->d_name, game_file))
      gtk_option_menu_set_history (GTK_OPTION_MENU (option_menu), i);
  
  sprintf (buf, "%d", seed);
  gtk_entry_set_text (GTK_ENTRY (seed_entry), buf);

  gnome_dialog_run_modal (GNOME_DIALOG (dialog));
}

void show_hint_dialog() 
{
  GtkWidget* dialog;
  GString* gmessage;

  if (game_over) {
    gmessage = g_string_new (_("The game is over.\nNo hints are available"));
  }  
  else {
    SCM hint = gh_apply(game_data->hint_lambda, gh_cons(SCM_EOL,SCM_EOL));

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

  gnome_app_message (GNOME_APP (app), gmessage->str);
}

GtkWidget *
get_main_prefs ()
{
  GtkWidget *retval;
  
  retval = gtk_vbox_new (FALSE, GNOME_PAD_SMALL);

  gtk_widget_show_all (retval);
  return retval;

}

GtkWidget *
get_card_prefs ()
{
  GtkWidget *retval;
}

GtkWidget *
get_background_prefs ()
{
  GtkWidget *retval;
  
}
void show_property_dialog () 
{
  static GtkWidget* dialog = NULL;

  if (!dialog) {
    dialog = gnome_property_box_new ();
    /*    gnome_property_box_append_page (get_main_prefs ());
    gnome_property_box_append_page (get_card_prefs ());
    gnome_property_box_append_page (get_background_prefs ());*/
  }
  
  gtk_widget_show_all (dialog);
}

void show_rules_options_dialog () 
{
}

void show_global_stats_dialog () 
{
}

void show_rules_stats_dialog () 
{
}
