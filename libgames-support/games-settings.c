/*
 *  Copyright © 2007, 2010 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope conf it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <config.h>

#include "games-settings.h"

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#define G_SETTINGS_ENABLE_BACKEND
#include <gio/gsettingsbackend.h>

#include "games-gtk-compat.h"
#include "games-debug.h"

#define WINDOW_STATE_TIMEOUT 1 /* s */

#define I_(string) g_intern_static_string (string)

#define SCHEMA_NAME           I_("org.gnome.Games.WindowState")

#define STATE_KEY_MAXIMIZED   I_("maximized")
#define STATE_KEY_FULLSCREEN  I_("fullscreen")
#define STATE_KEY_WIDTH       I_("width")
#define STATE_KEY_HEIGHT      I_("height")

typedef struct {
  GSettings *settings;
  GtkWindow *window;
  guint timeout_id;
  int width;
  int height;
  guint is_maximised : 1;
  guint is_fullscreen : 1;
} WindowState;

static gboolean
window_state_timeout_cb (WindowState *state)
{
  g_settings_set_int (state->settings, STATE_KEY_WIDTH, state->width);
  g_settings_set_int (state->settings, STATE_KEY_HEIGHT, state->height);

  _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                      "[window %p] timeout: persisting width:%d height:%d\n",
                      state->window,
                      state->width, state->height);

  state->timeout_id = 0;
  return FALSE;
}

static void
free_window_state (WindowState *state)
{
  if (state->timeout_id != 0) {
    g_source_remove (state->timeout_id);

    /* And store now */
    window_state_timeout_cb (state);
  }

  g_object_unref (state->settings);

  g_slice_free (WindowState, state);
}

static gboolean
window_configure_event_cb (GtkWidget *widget,
                           GdkEventConfigure *event,
                           WindowState *state)
{
  _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                      "[window %p] configure event current %dx%d new %dx%d [state: is-maximised:%s is-fullscreen:%s]\n",
                      state->window,
                      state->width, state->height,
                      event->width, event->height,
                      state->is_maximised ? "t" : "f",
                      state->is_fullscreen ? "t" : "f");

  if (!state->is_maximised && !state->is_fullscreen &&
      (state->width != event->width || state->height != event->height)) {
    state->width = event->width;
    state->height = event->height;

  _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                      "[window %p] scheduling save of new window size\n",
                      state->window);

    if (state->timeout_id == 0) {
      state->timeout_id = g_timeout_add_seconds (WINDOW_STATE_TIMEOUT,
                                                 (GSourceFunc) window_state_timeout_cb,
                                                 state);
    }
  }

  return FALSE;
}

static gboolean
window_state_event_cb (GtkWidget *widget,
                       GdkEventWindowState *event,
                       WindowState *state)
{
  _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                      "[window %p] state event, mask:%x new-state:%x current state: is-maximised:%s is-fullscreen:%s\n",
                      state->window,
                      event->changed_mask, event->new_window_state,
                      state->is_maximised ? "t" : "f",
                      state->is_fullscreen ? "t" : "f");

  if (event->changed_mask & GDK_WINDOW_STATE_MAXIMIZED) {
    state->is_maximised = (event->new_window_state & GDK_WINDOW_STATE_MAXIMIZED) != 0;
    g_settings_set_boolean (state->settings, STATE_KEY_MAXIMIZED, state->is_maximised);
  }
  if (event->changed_mask & GDK_WINDOW_STATE_FULLSCREEN) {
    state->is_fullscreen = (event->new_window_state & GDK_WINDOW_STATE_FULLSCREEN) != 0;
    g_settings_set_boolean (state->settings, STATE_KEY_FULLSCREEN, state->is_fullscreen);
  }

  _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                      "  > new state: is-maximised:%s is-fullscreen:%s\n",
                      state->is_maximised ? "t" : "f",
                      state->is_fullscreen ? "t" : "f");


  return FALSE;
}

#if 0 //#ifndef HAVE_HILDON

#define ACCELMAP_EXT "accels"

static char *
games_conf_get_accel_map_path (GamesConf *conf,
                               gboolean ensure_dir_exists)
{
  GamesConfPrivate *priv = conf->priv;
  char *game_name, *conf_dir;
  char *conf_file = NULL;
  const char *override;

  game_name = g_ascii_strdown (priv->game_name, -1);

#ifdef HAVE_GNOME
  override = g_getenv ("GNOME22_USER_DIR");
  if (override)
    conf_dir = g_build_filename (override, "accels", NULL);
  else
    conf_dir = g_build_filename (g_get_home_dir (), ".gnome2", "accels", NULL);
#else
  conf_dir = g_build_filename (g_get_user_config_dir (), "gnome-games", NULL);
#endif
  if (!conf_dir)
    goto loser;

  /* Mode 0700 per the XDG basedir spec */
  if (ensure_dir_exists &&
      g_mkdir_with_parents (conf_dir, 0700) < 0) {
    int err = errno;

    if (err != EEXIST) {
      g_warning ("Failed to create config directory \"%s\": %s\n", conf_dir, g_strerror (err));
      goto loser;
    }
  }

#ifdef HAVE_GNOME
  conf_file = g_build_filename (conf_dir, game_name, NULL);
#else
{
  char *accelmap_filename;

  accelmap_filename = g_strdup_printf ("%s.%s", game_name, ACCELMAP_EXT);
  conf_file = g_build_filename (conf_dir, accelmap_filename, NULL);
  g_free (accelmap_filename);
}
#endif

loser:
  g_free (conf_dir);
  g_free (game_name);

  return conf_file;
}

static void
games_conf_load_accel_map (GamesConf *conf)
{
  char *conf_file;

  conf_file = games_conf_get_accel_map_path (conf, FALSE);
  if (!conf_file)
    return;

  gtk_accel_map_load (conf_file);
  g_free (conf_file);
}

static void
games_conf_save_accel_map (GamesConf *conf)
{
  char *conf_file;

  conf_file = games_conf_get_accel_map_path (conf, TRUE);
  if (!conf_file)
    return;

  gtk_accel_map_save (conf_file);
  g_free (conf_file);
}

#endif /* !HAVE_HILDON */

/**
 * games_settings_new_for_state:
 * @path:
 *
 * Creates a #GSettings object to store window state.
 *
 * Returns: a new #GSettings
 */
GSettings *
games_settings_new_for_state (const char *game_name,
                              const char *path)
{
  char *filename;

  filename = g_build_filename (g_get_tmp_dir (),
                               /* FIXME: use g_get_user_cache_dir (),
                               game_name, */
                               "gsettings-state.ini",
                               NULL);
  g_settings_backend_setup_keyfile ("state", filename);
  g_free (filename);

  return g_settings_new_with_context_and_path (SCHEMA_NAME, "state", path);
}

/**
 * games_settings_get_keyval:
 * @settings: a #GSettings
 * @key: the key name
 * @keyval: (out):
 * @modifiers: (out):
 *
 * Returns the keyboard key associated with @key in @group, or 0 if
 * the value could not be parsed as a keyval.
 *
 * Returns: a keyboard key value
 */
void
games_settings_get_keyval (GSettings *settings,
                           const char *key,
                           guint *keyval,
                           GdkModifierType *modifiers)
{
  char *value;
  guint kv;
  GdkModifierType km;

  g_return_if_fail (G_IS_SETTINGS (settings));
  g_return_if_fail (key != NULL && key[0] != '\0');

  value = g_settings_get_string (settings, key);
  gtk_accelerator_parse (value, &kv, &km);
  g_free (value);

  if (kv == 0 && km == 0)
    kv = GDK_VoidSymbol;

  if (keyval)
    *keyval = kv;
  if (modifiers)
    *modifiers = km;
}

/**
 * games_settings_set_keyval:
 * @settings: a #GSettings
 * @key: the key name
 * @value: the value to store
 *
 * Associates @value with the key @key in group @group.
 *
 * It is a programmer error to pass a key that isn't valid for settings.
 *
 * Returns: TRUE if setting the key succeeded, FALSE if the key was not writable
 */
gboolean
games_settings_set_keyval (GSettings *settings,
                           const char *key,
                           guint keyval,
                           GdkModifierType modifiers)
{
  char *value;
  gboolean rv;

  g_return_val_if_fail (G_IS_SETTINGS (settings), FALSE);
  g_return_val_if_fail (key != NULL && key[0] != '\0', FALSE);

  value = gtk_accelerator_name (keyval, modifiers);
  rv = g_settings_set_string (settings, key, value);
  g_free (value);

  return rv;
}

/**
 * games_settings_bind_window_state:
 * @settings: a #GSettings with schema org.gnome.Games.WindowState
 * @window: a #GtkWindow
 *
 * Restore the window configuration, and persist changes to the window configuration:
 * window width and height, and maximised and fullscreen state.
 * @window must not be realised yet.
 */
void
games_settings_bind_window_state (GSettings *settings,
                                  GtkWindow *window)
{
  WindowState *state;
  int width, height;
  gboolean maximised, fullscreen;

  g_return_if_fail (GTK_IS_WINDOW (window));
  g_return_if_fail (!gtk_widget_get_realized (GTK_WIDGET (window)));

  state = g_slice_new0 (WindowState);

  state->window = window;
  state->settings = g_object_ref (settings);
  g_object_set_data_full (G_OBJECT (window), "GamesSettings::WindowState",
                          state, (GDestroyNotify) free_window_state);

  g_signal_connect (window, "configure-event",
                    G_CALLBACK (window_configure_event_cb), state);
  g_signal_connect (window, "window-state-event",
                    G_CALLBACK (window_state_event_cb), state);

  maximised = g_settings_get_boolean (settings, STATE_KEY_MAXIMIZED);
  fullscreen = g_settings_get_boolean (settings, STATE_KEY_FULLSCREEN);
  width = g_settings_get_int (settings, STATE_KEY_WIDTH);
  height = g_settings_get_int (settings, STATE_KEY_HEIGHT);

  if (width > 0 && height > 0) {
    _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                        "[window %p] restoring size %dx%d\n",
                        state->window,
                        width, height);
    gtk_window_set_default_size (GTK_WINDOW (window), width, height);
  }
  if (maximised) {
    _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                        "[window %p] restoring maximised state\n",
                        state->window);
    gtk_window_maximize (GTK_WINDOW (window));
  }
  if (fullscreen) {
    _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                        "[window %p] restoring fullscreen state\n",
                        state->window);
    gtk_window_fullscreen (GTK_WINDOW (window));
  }
}
