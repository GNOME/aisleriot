#ifndef GAMES_NETWORK_H
#define GAMES_NETWORK_H

extern char *game_server;
extern guint whose_turn;
const char *player_name;
const char *opponent_name;
extern guint game_in_progress;

typedef struct {
  char inbuf[1024];
  int inpos;

  int fd;
  GIOChannel *fd_gioc;
  guint read_tag, write_tag;
  int mycolor;
  int sent_newgame;
  int sent_startgame;

  enum { CONNECTING, CONNECTED, DISCONNECTED } status;

  GString *outbuf;
} NetworkGame;

extern int games_network_allow (void);
extern void games_network_new (char *server, char *port, 
                               GtkWidget *parent_window);
extern void games_network_start (void);
extern void games_network_stop (void);
extern void games_network_connect (const char *id);
void games_send_gamedata (const gchar *msg);

extern void network_set_status (NetworkGame *ng,
                                int status, const char *message);
extern int get_network_status (void);
int get_mycolor (void);

extern void set_game_input_cb (void (*game_input_cb)(NetworkGame *ng, char *buf));
extern void set_game_clear_cb (void (*func)(void)); 
extern void set_game_move_cb (gint (*func)(guint x, guint y, guint me)); 



#endif

