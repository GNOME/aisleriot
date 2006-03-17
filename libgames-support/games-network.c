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
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>


#if defined (HAVE_HOWL)
/* Workaround broken howl including config.h */
#undef PACKAGE
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef VERSION
#include <howl.h>

static sw_discovery discovery;
#elif defined (HAVE_BONJOUR)
#include <dns_sd.h>

static DNSServiceRef publish_session;
#elif defined (HAVE_AVAHI)
#include <avahi-client/client.h>
#include <avahi-client/lookup.h>
#include <avahi-client/client.h>
#include <avahi-client/publish.h>

#include <avahi-common/alternative.h>
#include <avahi-glib/glib-watch.h>
#include <avahi-glib/glib-malloc.h>
#include <avahi-common/malloc.h>
#include <avahi-common/error.h>

static AvahiEntryGroup *group = NULL;
static AvahiGLibPoll *glib_poll = NULL;
static AvahiClient *client = NULL;
static char *avahi_name = NULL;
static gboolean is_browsing = FALSE;
#endif

#include "games-network.h"
#include "games-network-dialog.h"

const char *player_name;
const char *opponent_name;

char *games_network_server_name;
static char *game_port;
static pid_t server_pid = - 1;

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

#ifdef HAVE_ZEROCONF
static void games_zeroconf_teardown (void);
#endif

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
#ifdef HAVE_ZEROCONF
  games_zeroconf_teardown ();
#endif
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
  game_clear_cb ();
  games_kill_server ();
#ifdef HAVE_ZEROCONF
  games_zeroconf_teardown ();
#endif
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
  x = getaddrinfo (games_network_server_name, game_port, &hints, &res);
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
games_network_connect (const char *server, const char *id)
{

  if (!id) {
    g_string_append_printf (netgame->outbuf, "start_game\n");
    netgame->sent_startgame = 1;
    network_io_setup (netgame, CALLER_OTHER);
    return;
  }

  player_name = id;
  games_network_start ();

  games_network_server_name = (char *)server;
  if (!games_network_server_name) {
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
  games_network_server_name = server;
  game_port = port;

  network_game_dialog_show (parent_window);
}

void
games_kill_server (void)
{
  if (server_pid != -1) {
    kill (server_pid, SIGTERM);
    server_pid = - 1;
  }
}

#ifdef HAVE_ZEROCONF
static void
games_zeroconf_teardown ()
{
#ifdef HAVE_BONJOUR
  if (publish_session != NULL) {
    DNSServiceRefDeallocate(publish_session);
    publish_session = NULL;
  }
#elif defined (HAVE_HOWL)
  if (discovery != NULL) {
    sw_discovery_stop_run(discovery);
    sw_discovery_fina(discovery);
  }
#elif defined (HAVE_AVAHI)
  if (client != NULL)
    avahi_client_free (client);
  if (glib_poll != NULL)
    avahi_glib_poll_free (glib_poll);
#endif
}

static void
games_start_server (char *name)
{
  int fd;

  games_kill_server ();

  /* Fork a new thread. One thread starts a new server,
     while the other thread continues handling the game. */

  server_pid = fork();

  if (server_pid == 0) {
    fclose(stdout);
    fclose(stderr);
    fclose(stdin);

    fd = open("/dev/null", O_RDONLY);
    if (fd != 0) {
      dup2(fd, 0);
    }
    /* Execute the server.  */
    execvp("games-server.py", NULL);
    /* This point will not be reached unless no server can be started. */
    network_gui_message ("Failed to fork server.");
    exit(1);
  }
  /* Make sure the server has started, before trying to connect to it.  */
  sleep(1);
  games_network_connect ("localhost", name);
}
#endif

#ifdef HAVE_BONJOUR
static void
publish_reply (DNSServiceRef sdRef,
         DNSServiceFlags flags,
         DNSServiceErrorType errorCode,
         const char *name,
         const char *regtype,
         const char *domain,
         void *context)
{
}
#elif defined HAVE_HOWL
static sw_result HOWL_API publish_reply(sw_discovery discovery,
                                        sw_discovery_oid oid,
                                        sw_discovery_publish_status status,
                                        sw_opaque extra) {
  char* errmsg = NULL;
  static sw_string
    status_text[] =
    {
      "Started",
      "Stopped",
      "Name Collision",
      "Invalid"
    };

  errmsg = g_strdup_printf ("publish reply: %s\n",
                            status_text[status]);
  g_message (errmsg);
  g_free (errmsg);

  return SW_OKAY;
}
#elif defined HAVE_AVAHI
static void create_services(AvahiClient *c);

static void
entry_group_callback(AvahiEntryGroup *g,
                     AvahiEntryGroupState state,
                     AVAHI_GCC_UNUSED void *userdata)
{

  g_assert(g == group);

  /* Called whenever the entry group state changes */

  switch (state)
  {
    case AVAHI_ENTRY_GROUP_ESTABLISHED :
      /* The entry group has been established successfully */
      break;

    case AVAHI_ENTRY_GROUP_COLLISION :
    {
      char *n;

      /* A service name collision happened. Let's pick a new name */
      n = avahi_alternative_service_name(avahi_name);
      avahi_free(avahi_name);
      avahi_name = n;

      /* And recreate the services */
      create_services(avahi_entry_group_get_client(g));
      break;
    }

    case AVAHI_ENTRY_GROUP_FAILURE :
    {
      /* Some kind of failure happened while we were registering our services */
      avahi_client_free (avahi_entry_group_get_client(g));
      avahi_glib_poll_free (glib_poll);
      break;
    }

    case AVAHI_ENTRY_GROUP_UNCOMMITED:
    case AVAHI_ENTRY_GROUP_REGISTERING:
    ;
  }
}

static void create_services(AvahiClient *c)
{
  char* mtype = NULL;
  char* errmsg = NULL;
  int ret;

  g_assert (c);

  mtype = g_strdup_printf ("_%s%s", game_port, NETWORK_ZEROCONF_TYPE);

  /* If this is the first time we're called, let's create a new entry group */
  if (!group)
    if (!(group = avahi_entry_group_new(c, entry_group_callback, NULL)))
    {
      errmsg = g_strdup_printf ("avahi_entry_group_new() failed: %s",
                                avahi_strerror(avahi_client_errno(c)));
      network_gui_message (_(errmsg));
      goto fail;
    }

  /* Add the service for gnome-games */
  if ((ret = avahi_entry_group_add_service(group,
                                           AVAHI_IF_UNSPEC,
                                           AVAHI_PROTO_UNSPEC,
                                           0,
                                           avahi_name,
                                           mtype,
                                           NULL,
                                           NULL,
                                           atoi (game_port),
                                           NULL)) < 0)
  {
    errmsg = g_strdup_printf ("Failed to add gnome-games service: %s",
                              avahi_strerror(ret));
    network_gui_message (_(errmsg));
    goto fail;
  }

  /* Tell the server to register the service */
  if ((ret = avahi_entry_group_commit(group)) < 0)
  {
    errmsg = g_strdup_printf ("Failed to commit entry_group: %s",
                              avahi_strerror(ret));
    network_gui_message (_(errmsg));
    goto fail;
  }

  g_free (mtype);
  return;

  fail:
    avahi_client_free (c);
    avahi_glib_poll_free (glib_poll);
    g_free (mtype);
    g_free (errmsg);
}

static void
client_callback(AvahiClient *c,
                AvahiClientState state,
                AVAHI_GCC_UNUSED void * userdata)
{
  char* errmsg = NULL;

  g_assert (c);

  /* Called whenever the client or server state changes */
  switch (state)
  {
    case AVAHI_CLIENT_S_RUNNING:

      /* The server has startup successfully and registered its host
       * name on the network, so it's time to create our services */
      if (!group && !is_browsing)
        create_services(c);
      break;

    case AVAHI_CLIENT_S_COLLISION:

      /* Let's drop our registered services. When the server is back
       * in AVAHI_SERVER_RUNNING state we will register them
       * again with the new host name. */
      if (group)
        avahi_entry_group_reset(group);
      break;

    case AVAHI_CLIENT_FAILURE:
    {
      errmsg = g_strdup_printf ("Client failure: %s",
                                avahi_strerror(avahi_client_errno(c)));
      network_gui_message (_(errmsg));

      /* Cleanup */
      avahi_client_free (c);
      avahi_glib_poll_free (glib_poll);
      g_free (errmsg);

      break;
    }

    case AVAHI_CLIENT_CONNECTING:
    case AVAHI_CLIENT_S_REGISTERING:
    ;
  }
}
#endif

gboolean
games_host_lan_game (char *name)
{
#if defined (HAVE_HOWL)
  gboolean retval = FALSE;
  sw_result result;
  sw_discovery_publish_id id;
  char* mtype = NULL;

  if (sw_discovery_init (&discovery) != SW_OKAY) {
    network_gui_message (_("Local Area Network game could not be started. \nTry running mDNSResponder."));
    return retval;
  }

  mtype = g_strdup_printf ("_%s%s", game_port, NETWORK_ZEROCONF_TYPE);

  if (!(result = sw_discovery_publish (discovery,
                                       0,
                                       name,
                                       mtype,
                                       NULL,
                                       NULL,
                                       atoi(game_port),
                                       NULL,
                                       0,
                                       publish_reply,
                                       NULL,
                                       &id)) != SW_OKAY) {
    games_start_server (name);
    retval = TRUE;
  }

  /* TODO: Check whether running the discovery blocks the main loop */
  sw_discovery_run(discovery);

  g_free(mtype);
  return retval;
#elif defined (HAVE_BONJOUR)
  gboolean retval = FALSE;
  DNSServiceErrorType err;
  char* mtype = NULL;
  fd_set set;
  int fd;
  struct timeval timeout;

  mtype = g_strdup_printf ("_%s%s", game_port, NETWORK_ZEROCONF_TYPE);

  err = DNSServiceRegister (&publish_session,
                            0 /* flags */,
                            0 /* interface; 0 for all */,
                            name /* name */,
                            mtype /* type */,
                            NULL /* domain */,
                            NULL /* hostname */,
                            g_htons (atoi (game_port)) /* port in network byte order */,
                            0, /* text record length */
                            NULL, /* text record */
                            publish_reply /* callback */,
                            NULL /* context */);

  if (err == kDNSServiceErr_NoError) {
    games_start_server (name);
    retval = TRUE;
  }

  /* Initialize the file descriptor set. */
  FD_ZERO (&set);
  FD_SET (fd, &set);

  /* Initialize the timeout data structure. */
  /* TODO: Should the value for sec be configurable? */
  timeout.tv_sec = 10;
  timeout.tv_usec = 0;

  /* TODO: Check whether the select blocks the main loop */
  if (publish_session != NULL) {
    fd = DNSServiceRefSockFD(publish_session);

    if (select(FD_SETSIZE,
                  &set, NULL, NULL,
                  &timeout) > 0) {
      DNSServiceProcessResult(publish_session);
    }
  }

  g_free (mtype);
  return retval;
#elif defined (HAVE_AVAHI)
  gboolean retval = FALSE;
  int error;
  char* errmsg = NULL;
  GMainLoop *loop = NULL;
  const AvahiPoll *poll_api;

  /* Optional: Tell avahi to use g_malloc and g_free */
  avahi_set_allocator (avahi_glib_allocator ());

  /* Create the GLIB main loop */
  loop = g_main_loop_new (NULL, FALSE);

  /* Create the GLIB Adaptor */
  glib_poll = avahi_glib_poll_new (NULL, G_PRIORITY_DEFAULT);
  poll_api = avahi_glib_poll_get (glib_poll);

  avahi_name = name;

  /* create a new client if necessary or use an existing one */
  if (!client)
    client = avahi_client_new(poll_api,
                              0,
                              client_callback,
                              loop,
                              &error);

  /* Check wether creating the client object succeeded */
  if (!client)
  {
    errmsg = g_strdup_printf ("Local Area Network game could not be started. \nFailed to create client: %s",
                              avahi_strerror (error));
    network_gui_message (_(errmsg));
    goto fail;
  }

  games_start_server (name);
  retval = TRUE;

  return retval;

  fail:
    /* Clean up */
    g_main_loop_unref (loop);
    avahi_client_free (client);
    avahi_glib_poll_free (glib_poll);
    g_free (errmsg);
    return retval;
#else
  return FALSE;
#endif
}

#if defined (HAVE_HOWL)

static sw_result HOWL_API
games_get_server (sw_discovery discovery, sw_discovery_oid oid,
     sw_uint32 interface_index, sw_const_string name, sw_const_string type,
     sw_const_string domain, sw_ipv4_address address, sw_port  port,
     sw_octets text_record, sw_uint32 text_record_len, sw_opaque_t extra)
{
  char name_buf[16];

  sw_discovery_cancel (discovery, oid);
  network_gui_add_server ((char *)name,
             (char *)sw_ipv4_address_name(address, name_buf, 16));

  return SW_OKAY;
}

static sw_result HOWL_API
games_browser (sw_discovery discovery, sw_discovery_oid oid,
               sw_discovery_browse_status status, sw_uint32 interface_index,
               sw_const_string name, sw_const_string type,
               sw_const_string domain, sw_opaque_t extra)
{
  sw_discovery_resolve_id resolve_id;

  if (status == SW_DISCOVERY_BROWSE_ADD_SERVICE)  {
    if (sw_discovery_resolve (discovery, interface_index, name,
        type, domain, games_get_server, NULL, &resolve_id) != SW_OKAY) {
      network_gui_message ("resolve failed\n");
    }
  }
  return SW_OKAY;
}

#elif defined (HAVE_BONJOUR)

static void
games_get_server (DNSServiceRef session,
                  DNSServiceFlags flags,
                  uint32_t interface_index,
                  DNSServiceErrorType error_code,
                  const char *full_name,
                  const char *host_name,
                  uint16_t port,
                  uint16_t text_record_len,
                  const char *text_record,
                  void *context)
{
  DNSServiceRefDeallocate (session);
  network_gui_add_server ((char *) host_name, (char *) host_name);
}

static void
games_browser (DNSServiceRef        session,
               DNSServiceFlags      flags,
               uint32_t             interface_index,
               DNSServiceErrorType  error_code,
               const char          *name,
               const char          *type,
               const char          *domain,
               void                *context)
{
  if (flags & kDNSServiceFlagsAdd) {
    DNSServiceRef resolve_session;
    DNSServiceErrorType res;

    res = DNSServiceResolve (&resolve_session,
                 0 /* flags (none needed) */,
                 0 /* interface (-1 for local, 0 for all) */,
                 name,
                 type,
                 domain,
                 games_get_server,
                 NULL);
    if (res != kDNSServiceErr_NoError) {
      network_gui_message ("resolve failed\n");
    }
  }
}

#elif defined (HAVE_AVAHI)

static void
games_get_server(
  AvahiServiceResolver *r,
  AVAHI_GCC_UNUSED AvahiIfIndex interface,
  AVAHI_GCC_UNUSED AvahiProtocol protocol,
  AvahiResolverEvent event,
  const char *name,
  const char *type,
  const char *domain,
  const char *host_name,
  const AvahiAddress *address,
  uint16_t port,
  AvahiStringList *txt,
  AvahiLookupResultFlags flags,
  AVAHI_GCC_UNUSED void* userdata)
{
  char* errmsg = NULL;

  g_assert (r);

  /* Called whenever a service has been resolved successfully or timed out */

  switch (event)
  {
    case AVAHI_RESOLVER_FAILURE:
    {
      errmsg = g_strdup_printf ("(Resolver) Failed to resolve service '%s' of type '%s' in domain '%s': %s\n",
                                name,
                                type,
                                domain,
                                avahi_strerror(avahi_client_errno(avahi_service_resolver_get_client(r))));
      network_gui_message (errmsg);
      break;
    }

    case AVAHI_RESOLVER_FOUND:
    {
      /* NOTE: If this particular gnome-games instance will
      be run on a system that does not utilize nss_mdns, you
      are required to use an IP address instead of a foobar.local
      address, because such an address won't be resolved.
      char a[AVAHI_ADDRESS_STR_MAX], *t;

      avahi_address_snprint(a, sizeof(a), address);
      */

      network_gui_add_server ((char *) host_name, (char *) host_name);
      break;
    }
  }

  avahi_service_resolver_free(r);
}

static void
games_browse(
  AvahiServiceBrowser *b,
  AvahiIfIndex interface,
  AvahiProtocol protocol,
  AvahiBrowserEvent event,
  const char *name,
  const char *type,
  const char *domain,
  AVAHI_GCC_UNUSED AvahiLookupResultFlags flags,
  void* userdata)
{
  char* errmsg = NULL;
  AvahiClient *c = userdata;

  g_assert (b);

  /* Called whenever a new services becomes available on the LAN or is removed from the LAN */

  switch (event)
  {
    case AVAHI_BROWSER_FAILURE:
    {
      errmsg = g_strdup_printf ("(Browser) %s\n",
                                avahi_strerror(avahi_client_errno(avahi_service_browser_get_client(b))));
      network_gui_message (errmsg);
      avahi_client_free (c);
      avahi_glib_poll_free (glib_poll);
      return;
    }

    case AVAHI_BROWSER_NEW:
    {
      errmsg = g_strdup_printf ("(Browser) NEW: service '%s' of type '%s' in domain '%s'\n",
                                name,
                                type,
                                domain);
      /* This is more a informative message,
         thus instead printing it to the GUI
         it should go to the console. */
      g_message (errmsg);

      /* We ignore the returned resolver object. In the callback
         function we free it. If the server is terminated before
         the callback function is called the server will free
         the resolver for us. */

      if (!(avahi_service_resolver_new(c,
                                       interface,
                                       protocol,
                                       name,
                                       type,
                                       domain,
                                       AVAHI_PROTO_UNSPEC,
                                       0,
                                       games_get_server,
                                       c)))
      {
        errmsg = g_strdup_printf ("Failed to resolve service '%s': %s\n",
                                  name,
                                  avahi_strerror(avahi_client_errno(c)));
        network_gui_message (errmsg);
      }

      break;
    }

    case AVAHI_BROWSER_REMOVE:
    {
      errmsg = g_strdup_printf ("(Browser) REMOVE: service '%s' of type '%s' in domain '%s'\n",
                                name,
                                type,
                                domain);
      network_gui_message (errmsg);
      break;
    }

    case AVAHI_BROWSER_ALL_FOR_NOW:
    case AVAHI_BROWSER_CACHE_EXHAUSTED:
    {
      errmsg = g_strdup_printf ("(Browser) %s\n",
                                event == AVAHI_BROWSER_CACHE_EXHAUSTED ? "CACHE_EXHAUSTED" : "ALL_FOR_NOW");
      /* this is more a informative message,
         thus print it to the console and not
         to the GUI. */
      g_message (errmsg);
      break;
    }
  }

  g_free(errmsg);
}

#endif

#if defined (HAVE_HOWL)

static gboolean
games_howl_input (GIOChannel *io_channel, GIOCondition cond,
          gpointer callback_data)
{
  sw_discovery session;
  sw_salt salt;

  session = callback_data;
  if (sw_discovery_salt (session, &salt) == SW_OKAY) {
    sw_salt_lock (salt);
    sw_discovery_read_socket (session);
    sw_salt_unlock (salt);
  }
  return TRUE;
}

#elif defined (HAVE_BONJOUR)

static gboolean
games_bonjour_input (GIOChannel *io_channel, GIOCondition cond,
                     gpointer callback_data)
{
  DNSServiceRef browse_session;

  browse_session = callback_data;
  DNSServiceProcessResult (browse_session);
  return TRUE;
}

#endif

gboolean
games_find_lan_game (void)
{
#if defined (HAVE_HOWL)
  sw_discovery discovery;
  sw_discovery_oid discovery_oid;
  GIOChannel *channel;
  static char mtype[256];
  int fd;

  if (sw_discovery_init(&discovery) != SW_OKAY) {
    network_gui_message (_("Local Area Network game could not be started. \nTry running mDNSResponder."));
    return FALSE;
  }

  snprintf (mtype, sizeof (mtype), "_%s%s", game_port, NETWORK_ZEROCONF_TYPE);

  if (sw_discovery_browse (discovery, 0, mtype, NULL,
              games_browser, NULL, &discovery_oid) != SW_OKAY) {
    network_gui_message (_("Local Area Network game could not be started. \nTry running mDNSResponder."));
    return FALSE;
  }

  fd = sw_discovery_socket (discovery);
  channel = g_io_channel_unix_new (fd);
  g_io_add_watch (channel, G_IO_IN, games_howl_input, discovery);
  g_io_channel_unref (channel);

  return TRUE;
#elif defined (HAVE_BONJOUR)
  DNSServiceRef browse_session;
  DNSServiceErrorType err;
  GIOChannel *channel;
  static char mtype[256];
  int fd;

  snprintf (mtype, sizeof (mtype), "_%s%s", game_port, NETWORK_ZEROCONF_TYPE);

  err = DNSServiceBrowse (&browse_session,
              0 /* flags (none needed) */,
              0 /* interface (-1 for local, 0 for all) */,
              mtype,
              "local",
              games_browser,
              NULL);

  if (err != kDNSServiceErr_NoError) {
    network_gui_message (_("Local Area Network game could not be started. \nTry running mDNSResponder."));
    return FALSE;
  }

  fd = DNSServiceRefSockFD (browse_session);
  channel = g_io_channel_unix_new (fd);
  g_io_add_watch (channel, G_IO_IN, games_bonjour_input, browse_session);
  g_io_channel_unref (channel);

  return TRUE;
#elif defined (HAVE_AVAHI)
  GMainLoop *loop = NULL;
  const AvahiPoll *poll_api;
  AvahiClient *client = NULL;
  AvahiServiceBrowser *sb = NULL;
  static char mtype[256];
  char* errmsg = NULL;
  int error;

  is_browsing = TRUE;

  snprintf (mtype, sizeof (mtype), "_%s%s", game_port, NETWORK_ZEROCONF_TYPE);

  /* Optional: Tell avahi to use g_malloc and g_free */
  avahi_set_allocator (avahi_glib_allocator ());

  /* Create the GLIB main loop */
  loop = g_main_loop_new (NULL, FALSE);

  /* Create the GLIB Adaptor */
  glib_poll = avahi_glib_poll_new (NULL, G_PRIORITY_DEFAULT);
  poll_api = avahi_glib_poll_get (glib_poll);

  /* Allocate a new client or reuse the existing one */
  if (!client)
    client = avahi_client_new(poll_api,
                              0,
                              client_callback,
                              loop,
                              &error);

  /* Check wether creating the client object succeeded */
  if (!client)
  {
    errmsg = g_strdup_printf ("Failed to create service browser: %s\n",
                              avahi_strerror (error));
    network_gui_message (_(errmsg));
    goto fail;
  }

  /* Create the service browser */
  if (!(sb = avahi_service_browser_new(client,
                                       AVAHI_IF_UNSPEC,
                                       AVAHI_PROTO_UNSPEC,
                                       mtype,
                                       "local",
                                       0,
                                       games_browse,
                                       client)))
  {
    errmsg = g_strdup_printf ("Failed to create service browser: %s\n",
                              avahi_strerror(avahi_client_errno(client)));
    network_gui_message (_(errmsg));
    goto fail;
  }

  return TRUE;

  fail:
    /* Clean up */
    g_main_loop_unref (loop);
    if (sb)
      avahi_service_browser_free(sb);
    if (client)
      avahi_client_free (client);
    avahi_glib_poll_free (glib_poll);
    g_free(errmsg);
    return FALSE;
#else
  return FALSE;
#endif
}
