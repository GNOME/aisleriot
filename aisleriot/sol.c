/*
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
 * Copyright © 2007 Andreas Røsdal <andreasr@gnome.org> 
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <string.h>

#include <libguile.h>

#include <glib.h>
#include <glib/gi18n.h>

#include <gtk/gtk.h>

#ifdef HAVE_CLUTTER
#include <cogl/cogl.h>
#include <clutter/clutter.h>
#include <clutter-gtk/clutter-gtk.h>

#ifndef CLUTTER_GTK_CHECK_VERSION
#define CLUTTER_GTK_CHECK_VERSION(a,b,c) (0)
#endif
#endif

#ifdef HAVE_HILDON
#include <libosso.h>

#ifdef HAVE_MAEMO_3
#include <hildon-widgets/hildon-program.h>
#else
#include <hildon/hildon-program.h>
#endif /* HAVE_MAEMO_3 */

#define SERVICE_NAME "org.gnome.Games.AisleRiot"
#endif /* HAVE_HILDON */

#include <libgames-support/games-debug.h>
#include <libgames-support/games-files.h>
#include <libgames-support/games-stock.h>
#include <libgames-support/games-runtime.h>
#include <libgames-support/games-sound.h>

#ifdef WITH_SMCLIENT
#include <libgames-support/eggsmclient.h>
#endif /* WITH_SMCLIENT */

#include "conf.h"
#include "game.h"
#include "window.h"

#if 0
/* String reserve */
N_("Solitaire")
N_("GNOME Solitaire")
N_("About Solitaire")
#endif /* 0 */

typedef struct {
  AisleriotWindow *window;
  char *variation;
  guint seed;
  gboolean freecell;
#ifdef HAVE_HILDON
  HildonProgram *program;
#endif /* HAVE_HILDON */
} AppData;

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

#ifdef WITH_SMCLIENT

static void
save_state_cb (EggSMClient *client,
               GKeyFile *key_file,
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
    argv[argc++] = (char *) "--freecell";
  } else {
    argv[argc++] = (char *) "--variation";
    argv[argc++] = (char *) game_name;
  }

  argv[argc++] = (char *) "--seed";
  argv[argc++] = seed;

  /* FIXMEchpe: save game state too? */

  egg_sm_client_set_restart_command (client, argc, (const char **) argv);

  g_free (seed);
}

static void
quit_cb (EggSMClient *client,
         AppData *data)
{
  /* This will cause gtk_main_quit */
  gtk_widget_destroy (GTK_WIDGET (data->window));
}

#endif /* WITH_SMCLIENT */

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

  games_conf_save ();

  if (state->memory_low_ind) {
    /* Run garbage collection */
    scm_gc ();
  }

#if GNOME_ENABLE_DEBUG
  if (state->shutdown_ind) {
    g_print ("Going to shut down\n");
  } else if (state->save_unsaved_data_ind) {
  } else if (state->memory_low_ind) {
    g_print ("Should try to free some memory\n");
  } else if (state->system_inactivity_ind) {
    g_print ("System inactive\n");
  }
#endif /* GNOME_ENABLE_DEBUG */
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
#endif /* GNOME_ENABLE_DEBUG */

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
    /* Ensure settings are saved to disk */
    games_conf_save ();

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
  GError *error = NULL;
  gboolean retval;
#ifdef WITH_SMCLIENT
  EggSMClient *sm_client;
#endif /* WITH_SMCLIENT */
#ifdef HAVE_MAEMO
  osso_hw_state_t hw_events = {
    TRUE /* shutdown */,
    TRUE /* save unsaved data */,
    TRUE /* low memory */,
    FALSE /* system inactivity */,
    OSSO_DEVMODE_NORMAL /* device mode */
    /* FIXMEchpe: or is OSSO_DEVMODE_INVALID the value to use
     * when not interested in this signal? The docs don't tell.
     */
  };
#endif /* HAVE_MAEMO */

  memset (&data, 0, sizeof (AppData));

#ifdef HAVE_MAEMO
  /* Set OSSO callbacks */
  if (osso_rpc_set_default_cb_f (games_runtime_get_osso_context (),
                                 osso_rpc_cb,
                                 &data) != OSSO_OK ||
      osso_hw_set_event_cb (games_runtime_get_osso_context (),
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
#endif /* GLIB 2.12.0 */

  add_main_options (option_context, &data);

  games_sound_enable (FALSE);

  g_option_context_add_group (option_context, gtk_get_option_group (TRUE));
#ifdef WITH_SMCLIENT
  g_option_context_add_group (option_context, egg_sm_client_get_option_group ());
#endif /* WITH_SMCLIENT */

#ifdef HAVE_CLUTTER
  g_option_context_add_group (option_context, cogl_get_option_group ());
  g_option_context_add_group (option_context, clutter_get_option_group_without_init ());
#if CLUTTER_GTK_CHECK_VERSION (0, 90, 0)
  g_option_context_add_group (option_context, gtk_clutter_get_option_group ());
#endif
#endif /* HAVE_CLUTTER */

#if defined(HAVE_HILDON) && defined(HAVE_MAEMO_5)
  {
    char *rc_file;

    /* Note: we have to use gtk_rc_add_default_file() before calling gtk_init() (via
     * g_option_context_parse()) rather than parsing the file directly afterward, in
     * order to get priority over the theme.
     */
    rc_file = games_runtime_get_file (GAMES_RUNTIME_GAME_DATA_DIRECTORY, "gtkrc-maemo");
    gtk_rc_add_default_file (rc_file);
    g_free (rc_file);
  }
#endif /* HAVE_HILDON && HAVE_MAEMO_5 */

  retval = g_option_context_parse (option_context, &argc, &argv, &error);
  g_option_context_free (option_context);

  if (!retval) {
    g_printerr ("%s\n", error->message);
    g_error_free (error);
    goto cleanup;
  }

#ifdef HAVE_CLUTTER
#if !CLUTTER_GTK_CHECK_VERSION (0, 90, 0)
  if (gtk_clutter_init_with_args (NULL, NULL, NULL, NULL, NULL, &error) != CLUTTER_INIT_SUCCESS) {
    g_printerr ("Failed to initialise clutter: %s\n", error->message);
    g_error_free (error);
    goto cleanup;
  }
#endif
#endif /* HAVE_CLUTTER */

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

  gtk_window_set_default_icon_name (data.freecell ? "gnome-freecell" : "gnome-aisleriot");

  data.window = AISLERIOT_WINDOW (aisleriot_window_new (data.freecell));
  g_signal_connect (data.window, "destroy",
		    G_CALLBACK (gtk_main_quit), NULL);

#ifdef WITH_SMCLIENT
  sm_client = egg_sm_client_get ();
  g_signal_connect (sm_client, "save-state",
		    G_CALLBACK (save_state_cb), &data);
  g_signal_connect (sm_client, "quit",
                    G_CALLBACK (quit_cb), &data);
#endif /* WITH_SMCLIENT */

#ifdef HAVE_HILDON
  hildon_program_add_window (data.program, HILDON_WINDOW (data.window));

  /* This is necessary since the setting is only installed
   * during class initialisation. See bug #585024.
   */
  /* For "gtk-menu-images" */
  g_type_class_unref (g_type_class_ref (GTK_TYPE_IMAGE_MENU_ITEM));
  /* For "gtk-button-images" */
  g_type_class_unref (g_type_class_ref (GTK_TYPE_BUTTON));
  /* For "gtk-toolbar-style" */
  g_type_class_unref (g_type_class_ref (GTK_TYPE_TOOLBAR));

  /* FIXMEchpe sort of strange that maemo doesn't all of this out-of-the-box... */
  g_object_set (gtk_widget_get_settings (GTK_WIDGET (data.window)),
                "gtk-alternative-button-order", TRUE,
                "gtk-toolbar-style", GTK_TOOLBAR_ICONS,
                "gtk-menu-images", FALSE,
                "gtk-button-images", FALSE,
#if GTK_CHECK_VERSION (2, 10, 0)
                "gtk-enable-mnemonics", FALSE,

                /* We want the default of FALSE for this property, but to work
                 * around https://bugs.maemo.org/show_bug.cgi?id=2278 we have
                 * to set this to TRUE.
                 */
                "gtk-enable-accels", TRUE,
#else
                "hildon-keyboard-shortcuts", FALSE,
#endif /* GTK 2.10.0 */
                NULL);
#endif /* HAVE_HILDON */

  if (data.freecell) {
    aisleriot_window_set_game (data.window, FREECELL_VARIATION, data.seed);
  } else {
    aisleriot_window_set_game (data.window, data.variation, data.seed);
  }

  gtk_window_present (GTK_WINDOW (data.window));

  gtk_main ();

  aisleriot_conf_shutdown ();

#ifdef WITH_SMCLIENT
  g_signal_handlers_disconnect_matched (sm_client, G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL, &data);
#endif /* WITH_SMCLIENT */

cleanup:
  g_free (data.variation);

#ifdef HAVE_MAEMO
  if (data.program != NULL) {
    g_object_unref (data.program);
  }
#endif /* HAVE_MAEMO */

  games_runtime_shutdown ();
}

int
main (int argc, char *argv[])
{
#ifndef HAVE_HILDON
  if (!games_runtime_init ("aisleriot"))
#else
  if (!games_runtime_init_with_osso ("aisleriot", SERVICE_NAME))
#endif /* !HAVE_HILDON */
    return 1;

  scm_boot_guile (argc, argv, main_prog, NULL); /* no return */

  return 0;
}
