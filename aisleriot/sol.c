/*
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
 * Copyright © 2007 Andreas Røsdal <andreasr@gnome.org> 
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#include "config.h"

#include <string.h>
#include <locale.h>

#include <libguile.h>

#include <glib/gi18n.h>
#include <glib/gthread.h>

#include <gtk/gtkaboutdialog.h>
#include <gtk/gtkicontheme.h>
#include <gtk/gtkmain.h>
#include <gtk/gtkmessagedialog.h>

#ifdef HAVE_GNOME
#include <libgnome/gnome-program.h>
#include <libgnomeui/gnome-app-helper.h>
#include <libgnomeui/gnome-client.h>
#include <libgnomeui/gnome-help.h>
#include <libgnomeui/gnome-ui-init.h>
#endif /* HAVE_GNOME */

#ifdef HAVE_HILDON
#include <libosso.h>

#ifdef HAVE_MAEMO_3
#include <osso-browser-interface.h>
#include <hildon-widgets/hildon-program.h>
#else
#include <hildon/hildon-program.h>
#endif /* HAVE_MAEMO_3 */

#define SERVICE_NAME "org.gnome.Games.AisleRiot"
#define HELP_EXT "xhtml"
#endif /* HAVE_HILDON */

#if defined (G_OS_WIN32)
#include <windows.h>
#include <io.h>
#define HELP_EXT "xhtml"
//FIXME On win32 help is created as html with gnome-doc-tool,
//and put manually in the directory below.
#define HELPDIR PKGDATADIR "/aisleriot/help"
#endif /* G_OS_WIN32 */

#include <libgames-support/games-files.h>
#include <libgames-support/games-stock.h>
#include <libgames-support/games-sound.h>

#include "conf.h"
#include "game.h"
#include "util.h"
#include "window.h"

typedef struct {
  AisleriotWindow *window;
  char *variation;
  guint seed;
  gboolean freecell;
#ifdef HAVE_GNOME
  GnomeProgram *program;
#endif
#ifdef HAVE_HILDON
  osso_context_t *osso_context;
  HildonProgram *program;
#endif
} AppData;

#ifndef HAVE_GNOME

static void
about_url_hook (GtkAboutDialog *about,
                const char *link,
                gpointer user_data)
{
#ifdef HAVE_MAEMO
  AppData *data = (AppData *) user_data;

  osso_rpc_run_with_defaults (data->osso_context,
                              "osso_browser",
                              OSSO_BROWSER_OPEN_NEW_WINDOW_REQ,
                              NULL,
                              DBUS_TYPE_STRING, link,
                              DBUS_TYPE_INVALID);

#elif defined (G_OS_WIN32)
  ShellExecute( NULL, "open", link, NULL, NULL, SW_SHOWNORMAL ); 
#else

  GdkScreen *screen;
  GError *error = NULL;
  char *argv[3] = { (char *) "gnome-open", (char *) link, NULL };

  screen = gtk_widget_get_screen (GTK_WIDGET (about));

  if (!gdk_spawn_on_screen (screen,
                            NULL /* working directory */,
                            argv,
                            NULL /* environment */,
                            G_SPAWN_SEARCH_PATH,
                            NULL, NULL,
                            NULL,
                            &error)) {
    GtkWidget *dialog;

    dialog = gtk_message_dialog_new (GTK_WINDOW (about),
                                     GTK_DIALOG_DESTROY_WITH_PARENT |
                                     GTK_DIALOG_MODAL,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     _("Could not show link"));
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                              "%s", error->message);
    g_error_free (error);

    gtk_window_set_title (GTK_WINDOW (dialog), "");
    gtk_window_set_resizable (GTK_WINDOW (dialog), FALSE);

    g_signal_connect (dialog, "response",
                      G_CALLBACK (gtk_widget_destroy), NULL);

    gtk_window_present (GTK_WINDOW (dialog));
  }
#endif /* HAVE_MAEMO */
}

#endif /* !HAVE_GNOME */

static char *
variation_to_game_file (const char *variation)
{
  char *game_file, *s;

  game_file = g_ascii_strdown (variation, -1);

  /* Replace dangerous characters: '.' (as in "..") and '/' */
  g_strdelimit (game_file, "." G_DIR_SEPARATOR_S, '\0');
  g_strdelimit (game_file, NULL, '_');

  if (game_file[0] == '\0') {
    g_free (game_file);
    return NULL;
  }

  /* Add the suffix */
  s = g_strconcat (game_file, ".scm", NULL);
  g_free (game_file);

  return s;
}

#ifdef HAVE_GNOME

static gboolean
save_yourself_cb (GnomeClient *client,
                  int phase,
                  GnomeSaveStyle save_style,
                  gboolean shutdown,
                  GnomeInteractStyle interact_style,
                  gboolean fast,
                  AppData *data)
{
  AisleriotGame *game;
  char *argv[5];
  const char *game_name;
  char *seed;
  int argc = 0;

  game = aisleriot_window_get_game (data->window);

  game_name = aisleriot_game_get_game_file (game);
  seed = g_strdup_printf ("%u", aisleriot_game_get_seed (game));

  argv[argc++] = g_get_prgname ();

  if (data->freecell) {
    argv[argc++] = "--freecell";
  } else {
    argv[argc++] = "--variation";
    argv[argc++] = (char *) game_name;
  }

  argv[argc++] = "--seed";
  argv[argc++] = seed;

  /* FIXMEchpe: save game state too? */

  gnome_client_set_restart_command (client, argc, argv);

  g_free (seed);

  return TRUE;
}

static void
die_cb (GnomeClient *client,
        AppData *data)
{
  /* This will cause gtk_main_quit */
  gtk_widget_destroy (GTK_WIDGET (data->window));
}

#endif /* HAVE_GNOME */

static char *
game_file_to_help_section (const char *game_file)
{
  char *p, *buf;

  buf = g_path_get_basename (game_file);

  if ((p = strrchr (buf, '.')))
    *p = '\0';
  for (p = buf; p = strchr (p, '-'), p && *p;)
    *p = '_';
  for (p = buf; p = strchr (p, '_'), p && *p;) {
    char *next = p + 1;
    char q = *next;

    if (q != '\0' && g_ascii_islower (q)) {
      *next = g_ascii_toupper (q);
      ++p;
    }
  }
  if (g_ascii_islower (buf[0])) {
    buf[0] = g_ascii_toupper (buf[0]);
  }

  return buf;
}

#if defined (HAVE_HILDON) || defined (G_OS_WIN32)

static void
help_hook (GtkWindow *parent,
           const char *game_file,
           gpointer user_data)
{
  AppData *data = (AppData *) user_data;
  char *help_section = NULL;
  char *help_url = NULL;
  guint i;
  const char * const *langs;

  if (game_file != NULL) {
    help_section = game_file_to_help_section (game_file);
  }

  langs = g_get_language_names ();

  for (i = 0; langs[i] != NULL; ++i) {
    const char *lang = langs[i];
    char *path;

    /* Filter out variants */
    if (strchr (lang, '.') != NULL ||
        strchr (lang, '@') != NULL)
      continue;

    path = games_path_runtime_fix (g_strdup_printf (HELPDIR G_DIR_SEPARATOR_S "%s" 
				   G_DIR_SEPARATOR_S "%s." HELP_EXT,
                                   lang,
                                   help_section ? help_section : "aisleriot"));
    if (g_file_test (path, G_FILE_TEST_EXISTS)) {
      help_url = g_strdup_printf ("file://%s", path);
      g_free (path);
      break;
    }

    g_free (path);
  }

  if (!help_url) {
    GtkWidget *dialog;

    dialog = gtk_message_dialog_new (parent,
                                     GTK_DIALOG_DESTROY_WITH_PARENT |
                                     GTK_DIALOG_MODAL,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     /* %s.%s is the game name + the extension HTML or XHTML, e.g. Klondike.html" */
                                     _("Help file \"%s.%s\" not found"),
                                     help_section ? help_section : "aisleriot",
                                     HELP_EXT);

    /* Empty title shows up as "<unnamed>" on maemo */
    gtk_window_set_title (GTK_WINDOW (dialog), _("Error"));
    gtk_window_set_resizable (GTK_WINDOW (dialog), FALSE);
    g_signal_connect (dialog, "response",
                      G_CALLBACK (gtk_widget_destroy), NULL);

    gtk_window_present (GTK_WINDOW (dialog));

    return;
  }

#ifdef HAVE_MAEMO
  osso_rpc_run_with_defaults (data->osso_context,
                              "osso_browser",
                              OSSO_BROWSER_OPEN_NEW_WINDOW_REQ,
                              NULL,
                              DBUS_TYPE_STRING, help_url,
                              DBUS_TYPE_INVALID);
#elif defined (G_OS_WIN32)
  ShellExecute( NULL, "open", help_url, NULL, NULL, SW_SHOWNORMAL ); 
#endif
  g_free (help_url);
}

#else /* !HAVE_MAEMO */

static void
help_hook (GtkWindow *parent,
           const char *game_file,
           gpointer user_data)
{
  GdkScreen *screen;
  char *help_section = NULL;
  GError *error = NULL;
#ifndef HAVE_GNOME
  char *help_url = NULL;
  char *argv[3];
#endif /* !HAVE_GNOME */

  screen = gtk_widget_get_screen (GTK_WIDGET (parent));

  if (game_file != NULL) {
    help_section = game_file_to_help_section (game_file);
  }

#ifdef HAVE_GNOME
  if (!gnome_help_display_on_screen ("aisleriot.xml", help_section, screen, &error)) {
#else

  if (help_section != NULL) {
    /* FIXMEchpe: URL-escape? */
    help_url = g_strdup_printf ("ghelp:aisleriot?%s", help_section);
  } else {
    help_url = g_strdup ("ghelp:aisleriot");
  }

  argv[0] = "gnome-open";
  argv[1] = help_url;
  argv[2] = NULL;

  if (!gdk_spawn_on_screen (screen,
                            NULL /* working directory */,
                            argv,
                            NULL /* environment */,
                            G_SPAWN_SEARCH_PATH,
                            NULL, NULL,
                            NULL,
                            &error)) {
#endif /* !HAVE_GNOME */
    GtkWidget *dialog;
    char *primary;

    if (game_file != NULL) {
      char *game_name;
 
      game_name = aisleriot_util_get_display_filename (game_file);
      primary = g_strdup_printf (_("Could not show help for \"%s\""), game_name);
      g_free (game_name);
    } else {
      primary = g_strdup (_("Could not show Aisleriot help"));
    }

    dialog = gtk_message_dialog_new (parent,
                                     GTK_DIALOG_DESTROY_WITH_PARENT |
                                     GTK_DIALOG_MODAL,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     "%s", primary);
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                              "%s", error->message);
    g_free (primary);
    g_error_free (error);

#ifdef HAVE_HILDON
  /* Empty title shows up as "<unnamed>" on maemo */
  gtk_window_set_title (GTK_WINDOW (dialog), _("Error"));
#else
  gtk_window_set_title (GTK_WINDOW (dialog), "");
#endif /* HAVE_HILDON */

    gtk_window_set_resizable (GTK_WINDOW (dialog), FALSE);

    g_signal_connect (dialog, "response",
                      G_CALLBACK (gtk_widget_destroy), NULL);

    gtk_window_present (GTK_WINDOW (dialog));
  }

  g_free (help_section);
#ifndef HAVE_GNOME
  g_free (help_url);
#endif /* !HAVE_GNOME */
}

#endif /* HAVE_HILDON */

#ifdef HAVE_MAEMO

static void
osso_hw_event_cb (osso_hw_state_t *state,
                  gpointer user_data)
{
  AppData *data = (AppData *) user_data;

  /* This callback can be called immediately upon registration.
   * So check if we're started up yet.
   */
  if (data->program == NULL)
    return;

#if GNOME_ENABLE_DEBUG
  if (state->shutdown_ind) {
    g_print ("Going to shut down\n");
  } else if (state->save_unsaved_data_ind) {
    g_print ("Should save unsaved data\n");
  } else if (state->memory_low_ind) {
    g_print ("Should try to free some memory\n");
  } else if (state->system_inactivity_ind) {
    g_print ("System inactive\n");
  }
#endif
}

static int
osso_rpc_cb (const char *interface,
             const char *method,
             GArray *args,
             gpointer user_data,
             osso_rpc_t *ret)
{
  AppData *data = (AppData *) user_data;

#if GNOME_ENABLE_DEBUG
  g_print ("OSSO RPC iface %s method %s\n", interface, method);
#endif

  if (strcmp (method, "top_application") == 0) {
    gtk_window_present (GTK_WINDOW (data->window));
  }

  ret->type = DBUS_TYPE_INVALID;
  return OSSO_OK;
}

static void
sync_is_topmost_cb (HildonProgram *program,
                    GParamSpec *pspec,
                    AppData *data)
{
  if (hildon_program_get_is_topmost (program)) {
    hildon_program_set_can_hibernate (program, FALSE);
  } else {
    /* FIXMEchpe: save state here */

    hildon_program_set_can_hibernate (program, TRUE);
  }
}

#endif /* HAVE_MAEMO */

static void
add_main_options (GOptionContext *option_context,
                  AppData *data)
{
  const GOptionEntry aisleriot_options[] = {
    { "variation", 'v', 0, G_OPTION_ARG_STRING, &data->variation,
      N_("Select the game type to play"), N_("NAME") },
    { "seed", 's', 0, G_OPTION_ARG_STRING, &data->seed,
      N_("Select the game number"), N_("NUMBER") },
    { "freecell", 0, G_OPTION_FLAG_HIDDEN, G_OPTION_ARG_NONE, &data->freecell,
      NULL, NULL },
    { NULL }
  };

  g_option_context_add_main_entries (option_context,
				     aisleriot_options, GETTEXT_PACKAGE);
}

static void
main_prog (void *closure, int argc, char *argv[])
{
  AppData data;
  GOptionContext *option_context;
  GtkIconTheme *sol_icon_theme;
#ifdef HAVE_GNOME
  GnomeClient *master_client;
#else
  GError *error = NULL;
  gboolean retval;
#endif
#ifdef HAVE_MAEMO
  osso_hw_state_t hw_events = {
    TRUE /* shutdown */,
    TRUE /* save unsaved data */,
    FALSE /* low memory */,
    FALSE /* system inactivity */,
    OSSO_DEVMODE_NORMAL /* device mode */
    /* FIXMEchpe: or is OSSO_DEVMODE_INVALID the value to use
     * when not interested in this signal? The docs don't tell.
     */
  };
#endif

  memset (&data, 0, sizeof (AppData));

#ifdef HAVE_MAEMO
  /* Set OSSO callbacks */
  /* NOTE: Passing the "org.gnome.games.aisleriot" service name to
   * osso_initialize as first parameter to specify the full service name
   * instead of getting com.nokia.* is undocumented.
   */
  data.osso_context = osso_initialize (SERVICE_NAME, VERSION, FALSE, NULL);
  if (!data.osso_context) {
    g_print ("Failed to initialise osso\n");
    goto cleanup;
  }

  if (osso_rpc_set_default_cb_f (data.osso_context,
                                 osso_rpc_cb,
                                 &data) != OSSO_OK ||
      osso_hw_set_event_cb (data.osso_context,
                            &hw_events,
                            osso_hw_event_cb,
                            &data) != OSSO_OK) {
    g_print ("Failed to connect OSSO handlers\n");
    goto cleanup;
  }
#endif /* HAVE_MAEMO */

  option_context = g_option_context_new (NULL);
#if GLIB_CHECK_VERSION (2, 12, 0)
  g_option_context_set_translation_domain (option_context, GETTEXT_PACKAGE);
#endif

  add_main_options (option_context, &data);

  games_sound_enable (FALSE);
  games_sound_add_option_group (option_context);

#ifdef HAVE_GNOME
  data.program = gnome_program_init ("aisleriot", VERSION,
                                     LIBGNOMEUI_MODULE,
                                     argc, argv,
                                     GNOME_PARAM_GOPTION_CONTEXT, option_context,
                                     GNOME_PARAM_APP_DATADIR, DATADIR,
                                     NULL);
#else /* !HAVE_GNOME */
  g_option_context_add_group (option_context, gtk_get_option_group (TRUE));

  retval = g_option_context_parse (option_context, &argc, &argv, &error);
  g_option_context_free (option_context);

  if (!retval) {
    g_print ("%s\n", error->message);
    goto cleanup;
  }
#endif /* HAVE_GNOME */

#ifdef HAVE_MAEMO
  data.program = HILDON_PROGRAM (hildon_program_get_instance ());

  g_signal_connect (data.program, "notify::is-topmost",
                    G_CALLBACK(sync_is_topmost_cb), &data);
#endif /* HAVE_MAEMO */

  g_set_application_name (data.freecell ? _("FreeCell Solitaire") : _("AisleRiot"));

  aisleriot_conf_init ();

  /* If we are asked for a specific game, check that it is valid. */
  if (!data.freecell &&
      data.variation != NULL) {
    char *game_file = NULL;

    if (data.variation[0] != '\0') {
      game_file = variation_to_game_file (data.variation);
    }

    g_free (data.variation);
    data.variation = game_file;
  }

  if (!data.freecell && !data.variation) {
    data.variation = games_conf_get_string_with_default (NULL, aisleriot_conf_get_key (CONF_VARIATION), DEFAULT_VARIATION);
  }

  g_assert (data.variation != NULL || data.freecell);

  games_stock_init ();

  aisleriot_util_set_help_func (help_hook, &data);

#if (!defined (HAVE_GNOME) || defined (G_OS_WIN32))
  gtk_about_dialog_set_url_hook (about_url_hook, &data, NULL);
#endif

  data.window = AISLERIOT_WINDOW (aisleriot_window_new ());
  g_signal_connect (data.window, "destroy",
		    G_CALLBACK (gtk_main_quit), NULL);

  sol_icon_theme = gtk_icon_theme_get_for_screen (gtk_widget_get_screen (GTK_WIDGET (data.window)));
  gtk_icon_theme_append_search_path (sol_icon_theme, games_path_runtime_fix (ICONDIR));

  gtk_window_set_default_icon_name (data.freecell ? "gnome-freecell" : "gnome-aisleriot");


#ifdef HAVE_GNOME
  master_client = gnome_master_client ();
  g_signal_connect (master_client, "save-yourself",
		    G_CALLBACK (save_yourself_cb), &data);
  g_signal_connect_swapped (master_client, "die",
                            G_CALLBACK (die_cb), &data);
#endif /* HAVE_GNOME */

#ifdef HAVE_HILDON
  hildon_program_add_window (data.program, HILDON_WINDOW (data.window));

  /* FIXMEchpe sort of strange that maemo doesn't all of this out-of-the-box... */
  g_object_set (gtk_widget_get_settings (GTK_WIDGET (data.window)),
                "gtk-alternative-button-order", TRUE,
                "gtk-menu-images", FALSE,
                "gtk-button-images", FALSE,
                "gtk-toolbar-style", (glong) GTK_TOOLBAR_ICONS,
#if GTK_CHECK_VERSION (2, 10, 0)
                "gtk-enable-accels", FALSE,
                "gtk-enable-mnemonics", FALSE,
#else
                "hildon-keyboard-shortcuts", FALSE,
#endif /* GTK 2.10.0 */
                NULL);
#endif /* HAVE_MAEMO */

  if (data.freecell) {
    aisleriot_window_set_freecell_mode (data.window);
    aisleriot_window_set_game (data.window, FREECELL_VARIATION, data.seed);
  } else {
    aisleriot_window_set_game (data.window, data.variation, data.seed);
  }

  gtk_window_present (GTK_WINDOW (data.window));

  gtk_main ();

  aisleriot_conf_shutdown ();

#ifndef HAVE_GNOME
cleanup:
#endif

  g_free (data.variation);

#ifdef HAVE_GNOME
  g_object_unref (data.program);
#endif /* HAVE_GNOME */

#ifdef HAVE_MAEMO
  if (data.program != NULL) {
    g_object_unref (data.program);
  }
  if (data.osso_context != NULL) {
    osso_deinitialize (data.osso_context);
  }
#endif /* HAVE_MAEMO */
}

int
main (int argc, char *argv[])
{
  char *localedir;

  setlocale (LC_ALL, "");

#if defined(HAVE_GNOME) || defined(HAVE_RSVG_GNOMEVFS) || defined(HAVE_GSTREAMER)
  /* If we're going to use gnome-vfs or gstreamer, we need to init threads before
   * calling any glib functions.
   */
  g_thread_init (NULL);
#endif

  localedir = games_path_runtime_fix (GNOMELOCALEDIR);
  bindtextdomain (GETTEXT_PACKAGE, localedir);
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain (GETTEXT_PACKAGE);
  g_free (localedir);

  scm_boot_guile (argc, argv, main_prog, NULL);

  return 0;
}
