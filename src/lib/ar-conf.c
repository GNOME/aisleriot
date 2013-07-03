/*
 *  Copyright © 2007 Christian Persch
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

#include "config.h"

#include <string.h>
#include <errno.h>

#include <gtk/gtk.h>

#define ACCELMAP_EXT "accels"

#include "ar-debug.h"
#include "ar-marshal.h"

#include "ar-conf.h"

#define AR_CONF_GET_PRIVATE(that)(G_TYPE_INSTANCE_GET_PRIVATE ((that), AR_TYPE_CONF, ArConfPrivate))

struct ArConfPrivate {
  char *game_name;
  GKeyFile *key_file;
  char *main_group;
  guint need_init : 1;
  guint dirty : 1;
};

enum
{
  PROP_0,
  PROP_GAME_NAME
};

enum
{
  VALUE_CHANGED,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static ArConf *instance;

G_DEFINE_TYPE (ArConf, ar_conf, G_TYPE_OBJECT);

/* helper functions */

#define WINDOW_STATE_TIMEOUT 1 /* s */

enum {
  STATE_KEY_MAXIMISED,
  STATE_KEY_FULLSCREEN,
  STATE_KEY_WIDTH,
  STATE_KEY_HEIGHT,
  LAST_STATE_KEY
};

static const char window_state_key_name[][12] = {
  "Maximised",
  "Fullscreen",
  "Width",
  "Height"
};

typedef struct {
  GtkWindow *window;
  char *group;
  guint timeout_id;
  int width;
  int height;
  guint is_maximised : 1;
  guint is_fullscreen : 1;
} WindowState;

static gboolean
window_state_timeout_cb (WindowState *state)
{
  ar_conf_set_integer (state->group, window_state_key_name[STATE_KEY_WIDTH], state->width);
  ar_conf_set_integer (state->group, window_state_key_name[STATE_KEY_HEIGHT], state->height);

  ar_debug_print (AR_DEBUG_WINDOW_STATE,
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

  g_free (state->group);

  g_slice_free (WindowState, state);
}

static gboolean
window_configure_event_cb (GtkWidget *widget,
                           GdkEventConfigure *event,
                           WindowState *state)
{
  ar_debug_print (AR_DEBUG_WINDOW_STATE,
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

  ar_debug_print (AR_DEBUG_WINDOW_STATE,
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
  ar_debug_print (AR_DEBUG_WINDOW_STATE,
                      "[window %p] state event, mask:%x new-state:%x current state: is-maximised:%s is-fullscreen:%s\n",
                      state->window,
                      event->changed_mask, event->new_window_state,
                      state->is_maximised ? "t" : "f",
                      state->is_fullscreen ? "t" : "f");

  if (event->changed_mask & GDK_WINDOW_STATE_MAXIMIZED) {
    state->is_maximised = (event->new_window_state & GDK_WINDOW_STATE_MAXIMIZED) != 0;
    ar_conf_set_boolean (state->group, window_state_key_name[STATE_KEY_MAXIMISED], state->is_maximised);
  }
  if (event->changed_mask & GDK_WINDOW_STATE_FULLSCREEN) {
    state->is_fullscreen = (event->new_window_state & GDK_WINDOW_STATE_FULLSCREEN) != 0;
    ar_conf_set_boolean (state->group, window_state_key_name[STATE_KEY_FULLSCREEN], state->is_fullscreen);
  }

  ar_debug_print (AR_DEBUG_WINDOW_STATE,
                      "  > new state: is-maximised:%s is-fullscreen:%s\n",
                      state->is_maximised ? "t" : "f",
                      state->is_fullscreen ? "t" : "f");


  return FALSE;
}

static char *
ar_conf_get_accel_map_path (ArConf *conf,
                               gboolean ensure_dir_exists)
{
  ArConfPrivate *priv = conf->priv;
  char *game_name, *conf_dir;
  char *conf_file = NULL;
  char *accelmap_filename;

  game_name = g_ascii_strdown (priv->game_name, -1);

  conf_dir = g_build_filename (g_get_user_config_dir (), "gnome-games", NULL);
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

  accelmap_filename = g_strdup_printf ("%s.%s", game_name, ACCELMAP_EXT);
  conf_file = g_build_filename (conf_dir, accelmap_filename, NULL);
  g_free (accelmap_filename);

loser:
  g_free (conf_dir);
  g_free (game_name);

  return conf_file;
}

static void
ar_conf_load_accel_map (ArConf *conf)
{
  char *conf_file;

  conf_file = ar_conf_get_accel_map_path (conf, FALSE);
  if (!conf_file)
    return;

  gtk_accel_map_load (conf_file);
  g_free (conf_file);
}

static void
ar_conf_save_accel_map (ArConf *conf)
{
  char *conf_file;

  conf_file = ar_conf_get_accel_map_path (conf, TRUE);
  if (!conf_file)
    return;

  gtk_accel_map_save (conf_file);
  g_free (conf_file);
}

static void
mark_dirty_cb (ArConf *conf)
{
  ArConfPrivate *priv = conf->priv;

  priv->dirty = TRUE;
}

/* Class implementation */

static void
ar_conf_init (ArConf *conf)
{
  ArConfPrivate *priv;

  priv = conf->priv = AR_CONF_GET_PRIVATE (conf);

  priv->need_init = FALSE;
  priv->dirty = FALSE;

  g_signal_connect (conf, "value-changed", G_CALLBACK (mark_dirty_cb), NULL);
}

static GObject *
ar_conf_constructor (GType type,
			  guint n_construct_properties,
			  GObjectConstructParam *construct_params)
{
  GObject *object;
  ArConf *conf;
  ArConfPrivate *priv;
  char *game_name;
  char *conf_file;
  GError *error = NULL;

  g_assert (instance == NULL);

  object = G_OBJECT_CLASS (ar_conf_parent_class)->constructor
             (type, n_construct_properties, construct_params);

  conf = AR_CONF (object);
  priv = conf->priv;

  g_assert (priv->game_name);

  game_name = g_ascii_strdown (priv->game_name, -1);

  priv->main_group = g_strdup_printf ("%s Config", priv->game_name);

  conf_file = g_build_filename (g_get_user_config_dir (), "gnome-games", game_name, NULL);

  priv->key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (priv->key_file, conf_file, 0, &error)) {
    /* Don't warn on non-existent file */
    if (!g_error_matches (error, G_FILE_ERROR, G_FILE_ERROR_NOENT)) {
      g_warning ("Failed to read settings from \"%s\": %s",
                  conf_file, error->message);
    }

    g_error_free (error);

    priv->need_init = TRUE;
  }

  g_free (conf_file);

  ar_conf_load_accel_map (conf);

  g_free (game_name);

  return object;
}

static void
ar_conf_finalize (GObject *object)
{
  ArConf *conf = AR_CONF (object);
  ArConfPrivate *priv = conf->priv;

  /* Save the accel map */
  ar_conf_save_accel_map (conf);

  ar_conf_save ();

  g_free (priv->main_group);
  g_key_file_free (priv->key_file);

  g_free (priv->game_name);

  G_OBJECT_CLASS (ar_conf_parent_class)->finalize (object);

  instance = NULL;
}

static void
ar_conf_set_property (GObject *object,
			   guint prop_id,
			   const GValue *value,
			   GParamSpec *pspec)
{
  ArConf *conf = AR_CONF (object);
  ArConfPrivate *priv = conf->priv;

  switch (prop_id) {
    case PROP_GAME_NAME:
      priv->game_name = g_value_dup_string (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
ar_conf_class_init (ArConfClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GType string_types[] = {
    G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE,
    G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE
  };

  gobject_class->constructor = ar_conf_constructor;
  gobject_class->finalize = ar_conf_finalize;
  gobject_class->set_property = ar_conf_set_property;

  signals[VALUE_CHANGED] =
    g_signal_newv ("value-changed",
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   ar_marshal_VOID__STRING_STRING,
                   G_TYPE_NONE,
                   2, string_types);

  g_object_class_install_property
    (gobject_class,
     PROP_GAME_NAME,
     g_param_spec_string ("game-name", NULL, NULL,
                          NULL,
                          G_PARAM_WRITABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB |
                          G_PARAM_CONSTRUCT_ONLY));

  g_type_class_add_private (gobject_class, sizeof (ArConfPrivate));
}

/* public API */

/**
 * ar_conf_initialise:
 * @game_name: the name of the game
 *
 * Initialises the default #ArConf instance.
 *
 * Returns: %TRUE if @game_name had saved settings; %FALSE otherwise
 */
gboolean
ar_conf_initialise (const char *game_name)
{
  instance = g_object_new (AR_TYPE_CONF,
                           "game-name", game_name,
                           NULL);

  return !instance->priv->need_init;
}

/**
 * ar_conf_shutdown:
 *
 * Shuts down the default #ArConf instance.
 */
void
ar_conf_shutdown (void)
{
  g_assert (instance != NULL);

  g_object_unref (instance);
  instance = NULL;
}

/**
 * ar_conf_get_default:
 *
 * Returns the default #ArConf instance. ar_conf_init() must have
 * been called before this!
 *
 * Returns: (transfer none): a #ArConf (no reference)
 */
ArConf *
ar_conf_get_default (void)
{
  g_assert (instance != NULL);

  return instance;
}

/**
 * ar_conf_save:
 *
 * Ensures settings are written to disk.
 */
void
ar_conf_save (void)
{
  ArConfPrivate *priv = instance->priv;
  char *game_name, *conf_file, *conf_dir, *data = NULL;
  gsize len = 0;
  GError *error = NULL;

  if (!priv->dirty)
    return;

  game_name = g_ascii_strdown (priv->game_name, -1);
  conf_dir = g_build_filename (g_get_user_config_dir (), "gnome-games", NULL);
  conf_file = g_build_filename (conf_dir, game_name, NULL);

  /* Ensure the directory exists; mode 0700 per the XDG basedir spec. */
  if (g_mkdir_with_parents (conf_dir, 0700) < 0) {
    int err = errno;

    if (err != EEXIST) {
      g_warning ("Failed to create config directory \"%s\": %s\n", conf_dir, g_strerror (err));
      goto loser;
    }
  }

  data = g_key_file_to_data (priv->key_file, &len, &error);
  if (!data) {
    g_warning ("Failed to save settings to \"%s\": %s",
               conf_file, error->message);
    g_error_free (error);
    goto loser;
  }

  if (!g_file_set_contents (conf_file, data, len, &error)) {
    g_warning ("Failed to save settings to \"%s\": %s",
              conf_file, error->message);
    g_error_free (error);
    goto loser;
  }

  /* Sucessfully saved */
  priv->dirty = FALSE;

loser:
  g_free (data);
  g_free (conf_file);
  g_free (conf_dir);
  g_free (game_name);
}

/**
 * ar_conf_get_string:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @error: (allow-none): a location for a #GError
 *
 * Returns the string associated with @key in @group, or %NULL if
 * @key is not set, or an error occurred
 *
 * Returns: a newly allocated string, or %NULL
 */
char *
ar_conf_get_string (const char *group, const char *key,
                       GError ** error)
{
  ArConfPrivate *priv = instance->priv;

  return g_key_file_get_string (priv->key_file, group ? group : priv->main_group, key, error);
}

/**
 * ar_conf_get_string_with_default:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @def_value: the default value
 *
 * Returns the string associated with @key in @group, or a copy of
 * @def_value if @key is not set, or an error occurred
 *
 * Returns: a newly allocated string
 */
char *
ar_conf_get_string_with_default (const char *group, const char *key,
                                    const char *def_value)
{
  GError *error = NULL;
  char *value;

  value = ar_conf_get_string (group, key, &error);
  if (value)
    return value;

  if (error) {
    g_error_free (error);
  }

  return g_strdup (def_value);
}

/**
 * ar_conf_set_string:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @value: the value to store
 *
 * Associates @value with the key @key in group @group.
 */
void
ar_conf_set_string (const char *group, const char *key,
                       const char *value)
{
  ArConfPrivate *priv = instance->priv;

  g_key_file_set_string (priv->key_file, group ? group : priv->main_group, key, value);
  g_signal_emit (instance, signals[VALUE_CHANGED], 0, group, key);
}

/**
 * ar_conf_get_string_list:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @n_values: a location to store the length of the returned array
 * @error: (allow-none): a location for a #GError
 *
 * Returns the string array associated with @key in @group, or %NULL if
 * @key is not set, or an error occurred
 *
 * Returns: (transfer full): a newly allocated string array, or %NULL
 */
char **
ar_conf_get_string_list (const char *group, const char *key,
                            gsize * n_values, GError ** error)
{
  ArConfPrivate *priv = instance->priv;

  return g_key_file_get_string_list (priv->key_file, group ? group : priv->main_group, key, n_values, error);
}

/**
 * ar_conf_set_string_list:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @values: the value to store
 * @n_values: the length of the @values array
 *
 * Associates @value with the key @key in group @group.
 */
void
ar_conf_set_string_list (const char *group, const char *key,
                            const char * const *values, gsize n_values)
{
  ArConfPrivate *priv = instance->priv;

  g_key_file_set_string_list (priv->key_file, group ? group : priv->main_group, key, values, n_values);
  g_signal_emit (instance, signals[VALUE_CHANGED], 0, group, key);
}

/**
 * ar_conf_get_integer:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @error: (allow-none): a location for a #GError
 *
 * Returns the integer associated with @key in @group, or 0 if
 * @key is not set, or an error occurred
 *
 * Returns: an integer
 */
int
ar_conf_get_integer (const char *group, const char *key,
                        GError ** error)
{
  ArConfPrivate *priv = instance->priv;

  return g_key_file_get_integer (priv->key_file, group ? group : priv->main_group, key, error);
}

/**
 * ar_conf_get_integer_with_default:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @def_value: the default value
 *
 * Returns the integer associated with @key in @group, or @def_value if
 * @key is not set, or an error occurred
 *
 * Returns: an integer
 */
int
ar_conf_get_integer_with_default (const char *group, const char *key,
                                     int def_value)
{
  GError *error = NULL;
  int value;

  value = ar_conf_get_integer (group, key, &error);
  if (error) {
    g_error_free (error);
    value = def_value;
  }

  return value;
}


/**
 * ar_conf_set_integer:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @value: the value to store
 *
 * Associates @value with the key @key in group @group.
 */
void
ar_conf_set_integer (const char *group, const char *key, int value)
{
  ArConfPrivate *priv = instance->priv;

  g_key_file_set_integer (priv->key_file, group ? group : priv->main_group, key, value);
  g_signal_emit (instance, signals[VALUE_CHANGED], 0, group, key);
}

/**
 * ar_conf_get_integer_list:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @n_values: a location to store the length of the returned array
 * @error: (allow-none): a location for a #GError
 *
 * Returns the integer associated with @key in @group, or 0 if
 * @key is not set, or an error occurred
 *
 * Returns: an integer
 */
int *
ar_conf_get_integer_list (const char *group, const char *key,
                             gsize * n_values, GError ** error)
{
  ArConfPrivate *priv = instance->priv;

  return g_key_file_get_integer_list (priv->key_file, group ? group : priv->main_group, key, n_values, error);
}

/**
 * ar_conf_set_integer_list:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @values: the value to store
 * @n_values: the length of the @values array
 *
 * Associates @value with the key @key in group @group.
 */
void
ar_conf_set_integer_list (const char *group, const char *key,
                             int *values, gsize n_values)
{
  ArConfPrivate *priv = instance->priv;

  g_key_file_set_integer_list (priv->key_file, group ? group : priv->main_group, key, values, n_values);
  g_signal_emit (instance, signals[VALUE_CHANGED], 0, group, key);
}

/**
 * ar_conf_get_boolean:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @error: (allow-none): a location for a #GError
 *
 * Returns the boolean associated with @key in @group, or %FALSE if
 * @key is not set, or an error occurred
 *
 * Returns: a boolean
 */
gboolean
ar_conf_get_boolean (const char *group, const char *key,
                        GError ** error)
{
  ArConfPrivate *priv = instance->priv;

  return g_key_file_get_boolean (priv->key_file, group ? group : priv->main_group, key, error);
}

/**
 * ar_conf_get_boolean_with_default:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @def_value: the default value
 *
 * Returns the boolean associated with @key in @group, or @def_value if
 * @key is not set, or an error occurred
 *
 * Returns: a boolean
 */
gboolean
ar_conf_get_boolean_with_default (const char *group, const char *key,
                                     gboolean def_value)
{
  GError *error = NULL;
  gboolean value;

  value = ar_conf_get_boolean (group, key, &error);
  if (error) {
    g_error_free (error);
    value = def_value;
  }

  return value;
}

/**
 * ar_conf_set_boolean:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @value: the value to store
 *
 * Associates @value with the key @key in group @group.
 */
void
ar_conf_set_boolean (const char *group, const char *key,
                        gboolean value)
{
  ArConfPrivate *priv = instance->priv;

  g_key_file_set_boolean (priv->key_file, group ? group : priv->main_group, key, value);
  g_signal_emit (instance, signals[VALUE_CHANGED], 0, group, key);
}

/**
 * ar_conf_get_double:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @error: a location for a #GError
 *
 * Returns the value associated with @key in @group, or 0 if
 * @key is not set, or an error occurred
 *
 * Returns: a double
 */
double
ar_conf_get_double (const char *group, const char *key,
                       GError ** error)
{
  ArConfPrivate *priv = instance->priv;

  return g_key_file_get_double (priv->key_file, group ? group : priv->main_group, key, error);
}

/**
 * ar_conf_set_double:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @value: the value to store
 *
 * Associates @value with the key @key in group @group.
 */
void
ar_conf_set_double (const char *group, const char *key, double value)
{
  ArConfPrivate *priv = instance->priv;

  g_key_file_set_double (priv->key_file, group ? group : priv->main_group, key, value);
  g_signal_emit (instance, signals[VALUE_CHANGED], 0, group, key);
}

/**
 * ar_conf_get_keyval:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @error: (allow-none): a location for a #GError
 *
 * Returns the keyboard key associated with @key in @group, or 0 if
 * @key is not set, or an error occurred
 *
 * Returns: a keyboard key value
 */
guint
ar_conf_get_keyval (const char *group, const char *key,
                       GError ** error)
{
  ArConfPrivate *priv = instance->priv;
  char *value;
  guint keyval = GDK_KEY_VoidSymbol;

  value = g_key_file_get_string (priv->key_file, group, key, error);
  if (value) {
    keyval = gdk_keyval_from_name (value);
    g_free (value);
  }

  return keyval;
}

/**
 * ar_conf_get_keyval_with_default:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @default_keyval: the default value
 *
 * Returns the keyboard key associated with @key in @group, or @default_keyval
 * if @key is not set, or an error occurred
 *
 * Returns: a keyboard key value
 */
guint
ar_conf_get_keyval_with_default (const char *group, const char *key,
                                 guint default_keyval)
{
  GError *error = NULL;
  guint value;

  value = ar_conf_get_keyval (group, key, &error);
  if (error) {
    g_error_free (error);
    value = default_keyval;
  }
  if (value == GDK_KEY_VoidSymbol) {
    value = default_keyval;
  }

  return value;
}

/**
 * ar_conf_set_keyval:
 * @group: (allow-none): the group name, or %NULL to use the default group
 * @key: the key name
 * @value: the value to store
 *
 * Associates @value with the key @key in group @group.
 */
void
ar_conf_set_keyval (const char *group, const char *key, guint value)
{
  ArConfPrivate *priv = instance->priv;
  char *name;

  if (value == GDK_KEY_VoidSymbol)
    return;
  
  name = gdk_keyval_name (value);
  g_key_file_set_string (priv->key_file, group, key, name);
  g_signal_emit (instance, signals[VALUE_CHANGED], 0, group, key);
}

/**
 * ar_conf_add_window:
 * @window: a #GtkWindow
 * @group: (allow-none): the group to store the state in, or %NULL to use
 * the default group
 * 
 * Restore the window configuration, and persist changes to the window configuration:
 * window width and height, and maximised and fullscreen state.
 * @window must not be realised yet.
 */
void
ar_conf_add_window (GtkWindow *window,
                       const char *group)
{
  WindowState *state;
  int width, height;
  gboolean maximised, fullscreen;

  g_return_if_fail (GTK_IS_WINDOW (window));
  g_return_if_fail (!gtk_widget_get_realized (GTK_WIDGET (window)));

  state = g_slice_new0 (WindowState);

  state->window = window;
  state->group = g_strdup (group);
  g_object_set_data_full (G_OBJECT (window), "ArConf::WindowState",
                          state, (GDestroyNotify) free_window_state);

  g_signal_connect (window, "configure-event",
                    G_CALLBACK (window_configure_event_cb), state);
  g_signal_connect (window, "window-state-event",
                    G_CALLBACK (window_state_event_cb), state);

  maximised = ar_conf_get_boolean (group, window_state_key_name[STATE_KEY_MAXIMISED], NULL);
  fullscreen = ar_conf_get_boolean (group, window_state_key_name[STATE_KEY_FULLSCREEN], NULL);
  width = ar_conf_get_integer (group, window_state_key_name[STATE_KEY_WIDTH], NULL);
  height = ar_conf_get_integer (group, window_state_key_name[STATE_KEY_HEIGHT], NULL);

  if (width > 0 && height > 0) {
    ar_debug_print (AR_DEBUG_WINDOW_STATE,
                        "[window %p] restoring size %dx%d\n",
                        state->window,
                        width, height);
    gtk_window_set_default_size (GTK_WINDOW (window), width, height);
  }
  if (maximised) {
    ar_debug_print (AR_DEBUG_WINDOW_STATE,
                        "[window %p] restoring maximised state\n",
                        state->window);
    gtk_window_maximize (GTK_WINDOW (window));
  }
  if (fullscreen) {
    ar_debug_print (AR_DEBUG_WINDOW_STATE,
                        "[window %p] restoring fullscreen state\n",
                        state->window);
    gtk_window_fullscreen (GTK_WINDOW (window));
  }
}
