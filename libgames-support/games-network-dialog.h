#ifndef GAMES_NETWORK_DIALOG_H
#define GAMES_NETWORK_DIALOG_H

extern void network_game_dialog_show (GtkWidget *parent_window);
extern void network_gui_message (const char *message);
extern void network_gui_add_player(const char *name);
extern void network_gui_close (void);
extern void network_gui_connected (void);
extern void set_game_msg_cb (void (*func)(gchar *message));

#endif
