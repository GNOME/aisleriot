#define DIALOG_C
#include "dialog.h"
#include "menu.h"

GtkWidget* game_over_dialog_box = NULL;
GtkWidget* load_game_dialog_box = NULL;
GtkWidget* dialog_box = NULL;
GtkWidget* won_lost_label = NULL;

int hide_box_callback (GtkWidget *app, void *data )
{
  gtk_widget_hide(game_over_dialog_box);
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

	 label = gtk_label_new (_("Game Over"));
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (game_over_dialog_box)->vbox), label, TRUE,
								TRUE, 0);
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (game_over_dialog_box)->vbox), won_lost_label, TRUE,
								TRUE, 0);

	 new_game_button = gtk_button_new_with_label (_("New Game"));
	 cancel_button = gtk_button_new_with_label (_("Cancel"));

	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (game_over_dialog_box)->action_area), new_game_button,
								TRUE, TRUE, 0);
	 gtk_box_pack_start (GTK_BOX (GTK_DIALOG (game_over_dialog_box)->action_area), cancel_button,
								TRUE, TRUE, 0);

	 gtk_signal_connect (GTK_OBJECT (cancel_button), "clicked",
								GTK_SIGNAL_FUNC (hide_box_callback), NULL);
	 gtk_signal_connect (GTK_OBJECT (new_game_button), "clicked",
								GTK_SIGNAL_FUNC (file_new_game_callback), NULL);

	 gtk_widget_show (label);
	 gtk_widget_show (won_lost_label);
	 gtk_widget_show (new_game_button);
	 gtk_widget_show (cancel_button);
  }
  if (won)
	 gtk_label_set ( won_lost_label, _("You Won!!!"));
  else
	 gtk_label_set ( won_lost_label, _("You Lost."));
  
  gtk_widget_show(game_over_dialog_box);
}

