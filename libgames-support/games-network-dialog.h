#ifndef GAMES_NETWORK_DIALOG_H
#define GAMES_NETWORK_DIALOG_H

#define KEY_NETWORK_NAME "/apps/gnome-games/network/name"
#define KEY_NETWORK_MODE "/apps/gnome-games/network/mode"
#define KEY_NETWORK_SERVER "/apps/gnome-games/network/server"
#define KEY_NETWORK_GAMENAME "/apps/gnome-games/network/gamename"

#define SERVER_MODE 1
#define HOST_LAN_MODE 2
#define JOIN_LAN_MODE 3

extern void network_game_dialog_show (GtkWidget *parent_window);
extern void network_gui_message (const char *message);
extern void network_gui_add_player(const char *name);
extern void network_gui_close (void);
extern void network_gui_connected (void);
extern void set_game_msg_cb (void (*func)(gchar *message));
extern void network_gui_add_server (char *name, char *address);

#endif
