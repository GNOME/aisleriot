#ifndef DIALOG_H
#define DIALOG_H
#include "gnome.h"

#ifndef DIALOG_C
extern GtkWidget* game_over_dialog_box;
extern GtkWidget* load_game_dialog_box;
extern GtkWidget* dialog_box;
#endif

int hide_box_callback (GtkWidget *app, void *data );
void show_game_over_dialog(gboolean);
void show_load_game_dialog();
void show_hint_dialog(char*);


#endif
