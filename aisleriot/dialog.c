#define DIALOG_C
#include <stdlib.h>
#include <guile/gh.h>
#include "dialog.h"
#include "sol.h"
#include "menu.h"
#include "cscmi.h"
#include "draw.h"

GtkWidget* game_over_dialog_box = NULL;
GtkWidget* select_game_dialog_box = NULL;
GtkWidget* dialog_box = NULL;
GtkWidget* won_lost_label = NULL;
GtkWidget* game_number_box;
GtkWidget *property_dialog = NULL;
void hide_game_over_box()
{
  if (game_over_dialog_box) {
	 gtk_grab_remove(game_over_dialog_box);
	 gtk_widget_hide(game_over_dialog_box);
  }
}

void hide_game_over_box_callback (GtkWidget *app, void *data )
{
  hide_game_over_box();
}

void hide_select_box()
{
  if (select_game_dialog_box){
	 gtk_grab_remove(select_game_dialog_box);
	 gtk_widget_hide(select_game_dialog_box);
  }
}

void hide_select_box_callback (GtkWidget *app, void *data )
{
  hide_select_box();
}

int select_game_callback (GtkWidget *app, void *data )
{
  score = 0;
  set_score();
  seed = atoi(gtk_entry_get_text(GTK_ENTRY(game_number_box)));
  srandom(seed);


  gh_apply(game_data->start_game_lambda, SCM_EOL);
  refresh_screen();

  if(surface) 
    timer_start();

  hide_select_box();
  make_title();

  return TRUE;
}

void show_game_over_dialog(gboolean won) {
  GtkWidget* new_game_button;
  GtkWidget* cancel_button;

  if (won_lost_label == NULL)
	 won_lost_label = gtk_label_new("");

  /* Create the dialog box if it doesn't already exist */
  if (!game_over_dialog_box) {
	 game_over_dialog_box = gtk_dialog_new();

	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (game_over_dialog_box)->vbox), won_lost_label, TRUE,
								TRUE, 0);

	 new_game_button = gtk_button_new_with_label (_("New Game"));
	 cancel_button = gnome_stock_button(GNOME_STOCK_BUTTON_CANCEL);

	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (game_over_dialog_box)->action_area), new_game_button,
								TRUE, TRUE, 0);
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (game_over_dialog_box)->action_area), cancel_button,
								TRUE, TRUE, 0);

	 gtk_signal_connect (GTK_OBJECT (cancel_button), "clicked",
								GTK_SIGNAL_FUNC (hide_game_over_box_callback), NULL);
	 gtk_signal_connect (GTK_OBJECT (new_game_button), "clicked",
								GTK_SIGNAL_FUNC (random_seed), NULL);

	 gtk_widget_show (won_lost_label);
	 gtk_widget_show (new_game_button);
	 gtk_widget_show (cancel_button);
  }
  if (won)
	 gtk_label_set ( GTK_LABEL(won_lost_label), _("Congratulations\n\nYou Won!!!"));
  else
	 gtk_label_set ( GTK_LABEL(won_lost_label), _("\nGame Over.\n"));
  gtk_grab_add (game_over_dialog_box);
  gtk_widget_show(game_over_dialog_box);
}

void show_select_game_dialog() {
  GtkWidget* label;
  GtkWidget* ok_button;
  GtkWidget* cancel_button;
  GString* seed_string;
  GtkWidget* hbox;

  if (!select_game_dialog_box) {
	 select_game_dialog_box = gtk_dialog_new();
	 label =  gtk_label_new("Select game number:\n");
	 hbox = gtk_hbox_new (FALSE, 0);
	 gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);
	 gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
	 gtk_container_border_width (GTK_CONTAINER(GTK_DIALOG (select_game_dialog_box)->vbox), GNOME_PAD);
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (select_game_dialog_box)->vbox), hbox, TRUE,
			     TRUE, 0);

	 /*set the entry box to be the score */
	 game_number_box = gtk_entry_new();
	 seed_string = g_string_new(NULL);
	 g_string_sprintf (seed_string, "%5d", seed);
	 
	 gtk_entry_set_text(GTK_ENTRY(game_number_box), seed_string->str);
	 gtk_entry_set_editable(GTK_ENTRY(game_number_box), TRUE);
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (select_game_dialog_box)->vbox), game_number_box, FALSE,
			     FALSE, 0);
	 /* buttons */
	 ok_button = gnome_stock_button(GNOME_STOCK_BUTTON_OK);
	 cancel_button = gnome_stock_button(GNOME_STOCK_BUTTON_CANCEL);

	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (select_game_dialog_box)->action_area), ok_button,
			     TRUE, TRUE, 0);
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (select_game_dialog_box)->action_area), cancel_button,
			     TRUE, TRUE, 0);

	 gtk_signal_connect (GTK_OBJECT (ok_button), "clicked",
			     GTK_SIGNAL_FUNC (select_game_callback), NULL);
	 gtk_signal_connect (GTK_OBJECT (cancel_button), "clicked",
			     GTK_SIGNAL_FUNC (hide_select_box_callback), NULL);

	 gtk_widget_show_all (GTK_DIALOG (select_game_dialog_box)->vbox);


  }
  /*GTK_WINDOW(select_game_dialog_box)->position = GTK_WIN_POS_MOUSE;*/
  gtk_window_set_modal (GTK_WINDOW (select_game_dialog_box), TRUE);
  gtk_widget_show(select_game_dialog_box);
}

void show_hint_dialog(char* message) {
  GtkWidget* hint_dialog;

  hint_dialog = gnome_message_box_new (message,GNOME_MESSAGE_BOX_QUESTION,
				       _("Ok"), NULL);
  /*GTK_WINDOW(hint_dialog)->position = GTK_WIN_POS_MOUSE;*/
	gtk_window_set_modal (GTK_WINDOW (hint_dialog), TRUE);
	gtk_widget_show (hint_dialog);
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
void show_property_dialog () {
  if (property_dialog == NULL) {
    property_dialog = gnome_property_box_new ();
    /*    gnome_property_box_append_page (get_main_prefs ());
    gnome_property_box_append_page (get_card_prefs ());
    gnome_property_box_append_page (get_background_prefs ());*/
  }
  
  gtk_widget_show_all (property_dialog);
}
