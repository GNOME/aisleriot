/* -*- mode:C; indent-tabs-mode:nil; tab-width:8; c-basic-offset:2; -*- */

/*
 * Games-network.c - Network code common to several gnome games. 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * For more details see the file COPYING.
 */


/* Originally was a lame CORBA-based implementation that was no use on the Internet.
   This is a simple line-oriented text protocol that you almost "can't get wrong".

   -- Elliot
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <gnome.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>
#include "games-network.h"
#include "games-network-dialog.h"

static char *game_server; 
static char *game_port;

guint game_in_progress = 0;
guint whose_turn;

/* Callback functions  */
void (*game_input_cb)(NetworkGame *ng, char *buf);
void (*game_clear_cb)(void);

static NetworkGame *netgame = NULL;
typedef enum { CALLER_READ_CB, CALLER_WRITE_CB, CALLER_OTHER } CallerType;

static gboolean network_handle_read (GIOChannel *source,
                                     GIOCondition cond, gpointer data);
static gboolean network_handle_write (GIOChannel *source,
                                      GIOCondition cond, gpointer data);
static void network_handle_input (NetworkGame *ng, char *buf);
static gboolean network_io_setup (NetworkGame *ng, CallerType caller);

int
get_network_status (void)
{
  if (netgame) {
    return netgame->status;
  } else {
    return 0;
  }
}

void 
set_game_input_cb (void (*func)(NetworkGame *ng, char *buf))
{
  game_input_cb = func;
}

void
set_game_clear_cb (void (*func)(void))
{
  game_clear_cb = func;
}


void
games_network_start (void)
{
  NetworkGame *nng;

  if (netgame)
    return;

  netgame = nng = g_new0 (NetworkGame, 1);
  nng->fd = -1;
  nng->outbuf = g_string_new ("");
  network_set_status (nng, DISCONNECTED,
                      _("Network initialization complete."));
}

void
games_network_stop (void)
{
  if (!netgame)
    return;

  network_set_status (netgame, DISCONNECTED,
                      _("Network shutdown in progress."));
  network_io_setup (netgame, CALLER_OTHER);
  g_string_free (netgame->outbuf, TRUE);
  g_free (netgame);
  netgame = NULL;
}

void
network_set_status (NetworkGame *ng, int status, const char *message)
{
  if (status != CONNECTED)
    game_in_progress = 0; 

  if (status == DISCONNECTED)
    {
      if (ng->fd >= 0)
	{
	  close (ng->fd); ng->fd = -1;
	}
      if (ng->fd_gioc)
	{
	  g_io_channel_unref (ng->fd_gioc); ng->fd_gioc = NULL;
	}
      g_string_truncate (netgame->outbuf, 0);
    }

  ng->status = status;
  ng->mycolor = 0;

  network_gui_message ((char *)message);
}

int
get_mycolor (void)
{
  if (netgame) {
    return netgame->mycolor;
  } else {
    return 0;
  }
}

static gboolean
network_io_setup (NetworkGame *ng, CallerType caller)
{
  gboolean need_read = FALSE, need_write = FALSE, retval = TRUE;

  if (ng->status == CONNECTING)
    {
      need_write = TRUE;
      need_read = FALSE;
    }
  else if (ng->status == CONNECTED)
    {
      need_read = TRUE;
      need_write = ng->outbuf->len?TRUE:FALSE;
    }

  if (need_read && !ng->read_tag)
    {
      ng->read_tag = g_io_add_watch (ng->fd_gioc,
                                     G_IO_IN | G_IO_ERR | G_IO_HUP | G_IO_NVAL,
                                     network_handle_read, ng);
    }
  else if (!need_read && ng->read_tag)
    {
      if (caller == CALLER_READ_CB)
	retval = FALSE;
      else
	g_source_remove (ng->read_tag);
      ng->read_tag = 0;
    }

  if (need_write && !ng->write_tag)
    {
      ng->write_tag = g_io_add_watch (ng->fd_gioc, G_IO_OUT,
                                      network_handle_write, ng);
    }
  else if (!need_write && ng->write_tag)
    {
      if (caller == CALLER_WRITE_CB)
	retval = FALSE;
      else
	g_source_remove (ng->write_tag);
      ng->write_tag = 0;
    }

  return retval;
}

static gboolean
network_handle_read (GIOChannel *source, GIOCondition cond, gpointer data)
{
  NetworkGame *ng = data;
  int maxread, n;
  char *ctmp;

  maxread = sizeof (ng->inbuf) - ng->inpos - 2;

  if (!(cond & G_IO_IN) || !maxread)
    goto errout;

  n = read (ng->fd, ng->inbuf + ng->inpos, maxread);
  if (n <= 0)
    goto errout;

  ng->inpos += n;
  ng->inbuf[ng->inpos] = '\0';
  while ((ctmp = strchr (ng->inbuf, '\n')))
    {
      int itmp;
      *(ctmp++) = '\0';
      network_handle_input (ng, ng->inbuf);
      itmp = ng->inpos - (ctmp - ng->inbuf);
      memmove (ng->inbuf, ctmp, itmp);
      ng->inpos -= ctmp - ng->inbuf;
      ng->inbuf[ng->inpos] = '\0';
    }

  return network_io_setup (ng, CALLER_READ_CB);

 errout:     
  /* Shut down the connection, either it was broken or someone is messing with us */
  network_set_status (ng, DISCONNECTED, _("The remote player disconnected"));
  return network_io_setup (ng, CALLER_READ_CB);
}

static gboolean
network_handle_write (GIOChannel *source, GIOCondition cond, gpointer data)
{
  NetworkGame *ng = data;
  int n;

  if (ng->status == CONNECTING)
    {
      int errval;
      socklen_t optlen = sizeof (errval);
      if (getsockopt (ng->fd, SOL_SOCKET, SO_ERROR, &errval, &optlen))
	g_error ("getsockopt failed!");

      if (errval)
	network_set_status (ng, DISCONNECTED, _("Error occurred during connect attempt."));
      else
	network_set_status (ng, CONNECTED, _("Connection succeeded, waiting for opponent"));
	
      return network_io_setup (ng, CALLER_WRITE_CB);
    }

  g_assert (ng->outbuf->len);
  g_assert (ng->status == CONNECTED);
  n = write (ng->fd, ng->outbuf->str, ng->outbuf->len);
  if (n <= 0)
    {
      network_set_status (ng, DISCONNECTED, _("Error occurred during write."));
    }
  else
    g_string_erase (ng->outbuf, 0, n);

  return network_io_setup (ng, CALLER_WRITE_CB);
}

static void
network_handle_input (NetworkGame *ng, char *buf)
{
  game_input_cb (ng, buf);
}

void
games_send_gamedata (const gchar *msg)
{
  NetworkGame *ng = netgame;

  if (ng) {
    if (ng->status == CONNECTED) {
      g_string_append_printf (ng->outbuf, msg);
      network_io_setup (ng, CALLER_OTHER);
    }
  }
}

int
games_network_allow (void)
{
  if (netgame && netgame->mycolor)
    return whose_turn == netgame->mycolor; 

  return !netgame;
}

static void
network_connect (void)
{
  int x;
  struct addrinfo *res = NULL, hints;

  g_string_truncate (netgame->outbuf, 0);
  netgame->inpos = 0;

  memset (&hints, 0, sizeof(hints));
  hints.ai_socktype = SOCK_STREAM;
  x = getaddrinfo (game_server, game_port, &hints, &res);
  if (x) {
    network_set_status (netgame, DISCONNECTED, gai_strerror(x));
    return;
  }

  if (netgame->status != DISCONNECTED)
    {
      network_set_status (netgame, DISCONNECTED, _("Cleaning up connection"));
      network_io_setup (netgame, CALLER_OTHER);
    }

  netgame->fd = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP);
  g_assert (netgame->fd >= 0);
  fcntl (netgame->fd, F_SETFL, O_NONBLOCK);
  netgame->fd_gioc = g_io_channel_unix_new (netgame->fd);
  x = connect (netgame->fd, res->ai_addr, res->ai_addrlen);
  if (x)
    {
      if (errno == EINPROGRESS)
	network_set_status (netgame, CONNECTING, _("Connection in progress..."));
      else
	{
	  perror("gnothello");
	  network_set_status (netgame, DISCONNECTED, _("Connection failed"));
	}
    }
  else
    network_set_status (netgame, CONNECTED,
                        _("Connection succeeded, waiting for opponent"));

  network_io_setup (netgame, CALLER_OTHER);

  freeaddrinfo (res);
}

void 
games_network_connect(const char *id) 
{

  if (!id) {
    g_string_append_printf (netgame->outbuf, "start_game\n");
    netgame->sent_startgame = 1;
    network_io_setup (netgame, CALLER_OTHER);
    return;
  }

  player_name = id;
  games_network_start (); 

  if (!game_server) {
    network_set_status (netgame, DISCONNECTED, _("No game server defined"));
    return;
  }

  if (netgame->status != CONNECTED)
    network_connect ();

  game_clear_cb ();
  
 
  g_string_append_printf (netgame->outbuf, "new_game %s \n", player_name);
  netgame->sent_newgame = 1;

  network_io_setup (netgame, CALLER_OTHER);
}


void 
games_network_new (char *server, char *port, GtkWidget *parent_window)
{
  game_server = server;
  game_port = port;

  network_game_dialog_show (parent_window);
}

