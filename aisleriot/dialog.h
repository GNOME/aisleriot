#ifndef DIALOG_H
#define DIALOG_H
#include "gnome.h"

void hide_game_over_box_callback (GtkWidget *app, void *data );
void hide_select_box_callback (GtkWidget *app, void *data );
void hide_game_over_box();
void hide_select_box();
void show_select_game_dialog(void);
void show_game_over_dialog(gboolean);
void show_load_game_dialog();
void show_hint_dialog(char*);


#endif
