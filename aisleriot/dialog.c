#define DIALOG_C
#include <stdlib.h>
#include <guile/gh.h>
#include "dialog.h"
#include "sol.h"
#include "menu.h"
#include "cscmi.h"

GtkWidget* game_over_dialog_box = NULL;
GtkWidget* select_game_dialog_box = NULL;
GtkWidget* dialog_box = NULL;
GtkWidget* won_lost_label = NULL;
GtkWidget* game_number_box;

void hide_game_over_box()
{
  if (game_over_dialog_box) {
	 gtk_grab_remove(game_over_dialog_box);
	 gtk_widget_hide(game_over_dialog_box);
  }
}
int hide_game_over_box_callback (GtkWidget *app, void *data )
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
int hide_select_box_callback (GtkWidget *app, void *data )
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

  hide_select_box();
  make_title();

  return TRUE;
}

void show_game_over_dialog(gboolean won) {
  GtkWidget* label;
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
								GTK_SIGNAL_FUNC (file_new_game_callback), NULL);

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

  if (!select_game_dialog_box) {
	 select_game_dialog_box = gtk_dialog_new();
	 label =  gtk_label_new("Select game number:\n");
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (select_game_dialog_box)->vbox), label, TRUE,
								TRUE, 0);

	 
	 /*set the entry box to be the score */
	 game_number_box = gtk_entry_new();
	 seed_string = g_string_new(NULL);
	 g_string_sprintf (seed_string, "%5d", seed);
	 
	 gtk_entry_set_text(GTK_ENTRY(game_number_box), seed_string->str);
	 gtk_entry_set_editable(GTK_ENTRY(game_number_box), TRUE);
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (select_game_dialog_box)->vbox), game_number_box, TRUE,
								TRUE, 0);
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

	 gtk_widget_show(label);
	 gtk_widget_show(game_number_box);
	 gtk_widget_show(ok_button);
	 gtk_widget_show(cancel_button);

  }
  gtk_grab_add(select_game_dialog_box);
  gtk_widget_show(select_game_dialog_box);
}

void show_hint_dialog(char* message) {
  GtkWidget* hint_dialog;

  hint_dialog = gnome_message_box_new (message,
												  GNOME_MESSAGE_BOX_QUESTION,
												  _("Ok"), NULL);
	GTK_WINDOW(hint_dialog)->position = GTK_WIN_POS_MOUSE;
	gnome_message_box_set_modal (GNOME_MESSAGE_BOX (hint_dialog));
	gtk_widget_show (hint_dialog);
}
