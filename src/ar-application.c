/*
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007, 2013 Christian Persch
 * Copyright © 2007 Andreas Røsdal <andreasr@gnome.org>
 * Copyright © 2013 William Jon McCann
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
#define G_SETTINGS_ENABLE_BACKEND

#include "ar-application.h"
#include "ar-defines.h"
#include "ar-runtime.h"

#include <errno.h>
#include <stdlib.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gio/gio.h>
#include <gio/gsettingsbackend.h>
#include <gtk/gtk.h>

#ifdef HAVE_CLUTTER
#include <cogl/cogl.h>
#include <clutter/clutter.h>
#include <clutter-gtk/clutter-gtk.h>
#endif

#include "ar-debug.h"
#include "ar-stock.h"
#include "ar-sound.h"
#include "ar-string-utils.h"

#include "window.h"
#include "game.h"
#include "util.h"

struct _ArApplicationClass {
  GtkApplicationClass parent_class;
};

struct _ArApplication {
  GtkApplication parent_instance;

  /*< private >*/
  ArApplicationPrivate *priv;
};

struct _ArApplicationPrivate
{
  AisleriotWindow *window;
  char *variation;
  gint seed; /* unused */
  gboolean freecell; /* unused */

  GSettingsBackend *state_keyfile_backend;
  GSettingsBackend *scores_keyfile_backend;
};

#if !GTK_CHECK_VERSION (3, 6, 0)
#define gtk_application_get_active_window(w) NULL
#endif

G_DEFINE_TYPE (ArApplication, ar_application, GTK_TYPE_APPLICATION)

static void
action_new_game (GSimpleAction *action,
                 GVariant      *parameter,
                 gpointer       user_data)
{
  GtkApplication *application = user_data;
  GtkWindow *window;

  window = gtk_application_get_active_window (application);
  if (!AISLERIOT_IS_WINDOW (window))
    return;

  aisleriot_window_new_game (AISLERIOT_WINDOW (window));
}

static void
action_change_game (GSimpleAction *action,
                    GVariant      *parameter,
                    gpointer       user_data)
{
  GtkApplication *application = user_data;
  GtkWindow *window;

  window = gtk_application_get_active_window (application);
  if (!AISLERIOT_IS_WINDOW (window))
    return;

  aisleriot_window_change_game (AISLERIOT_WINDOW (window));
}

static void
action_fullscreen (GSimpleAction *action,
                   GVariant      *parameter,
                   gpointer       user_data)
{
  GtkApplication *application = user_data;
  GtkWindow *window;
  GdkWindowState state;

  window = gtk_application_get_active_window (application);
  if (!AISLERIOT_IS_WINDOW (window))
    return;

  state = gdk_window_get_state (gtk_widget_get_window (GTK_WIDGET (window)));
  if (state & GDK_WINDOW_STATE_FULLSCREEN)
    gtk_window_unfullscreen (window);
  else
    gtk_window_fullscreen (window);
}

static void
action_statistics (GSimpleAction *action,
                   GVariant      *parameter,
                   gpointer       user_data)
{
  GtkApplication *application = user_data;
  GtkWindow *window;

  window = gtk_application_get_active_window (application);
  if (!AISLERIOT_IS_WINDOW (window))
    return;

  aisleriot_window_show_statistics_dialog (AISLERIOT_WINDOW (window));
}

static void
action_about (GSimpleAction *action,
              GVariant      *parameter,
              gpointer       user_data)
{
  GtkApplication *application = user_data;
  GtkWindow *window;

  window = gtk_application_get_active_window (application);
  aisleriot_window_show_about_dialog (AISLERIOT_WINDOW (window));
}

static void
action_help (GSimpleAction *action,
             GVariant      *parameter,
             gpointer       user_data)
{
  GtkApplication *application = user_data;
  GtkWindow *window;
  const char *game_module;

  window = gtk_application_get_active_window (application);
  if (!AISLERIOT_IS_WINDOW (window))
    return;

  game_module = aisleriot_window_get_game_module (AISLERIOT_WINDOW (window));
  aisleriot_show_help (GTK_WIDGET (window), game_module);
}

static void
action_quit (GSimpleAction *simple,
             GVariant *parameter,
             gpointer user_data)
{
  ArApplication *self = AR_APPLICATION (user_data);

  gtk_widget_destroy (GTK_WIDGET (self->priv->window));
}

static void
ar_application_activate (GApplication *application)
{
  ArApplication *self = AR_APPLICATION (application);

  gtk_window_present (GTK_WINDOW (self->priv->window));
}

static GActionEntry app_entries[] = {
  { "new-game", action_new_game, NULL, NULL, NULL },
  { "change-game", action_change_game, NULL, NULL, NULL },
  { "statistics", action_statistics, NULL, NULL, NULL },
  { "fullscreen", action_fullscreen, NULL, NULL, NULL },
  { "about", action_about, NULL, NULL, NULL },
  { "help", action_help, NULL, NULL, NULL },
  { "quit", action_quit, NULL, NULL, NULL },
};

static void
ar_application_startup (GApplication *application)
{
  const struct { 
    const char *action_name;
    const char *accel;
    const char *parameter_type;
    const char *parameter_string;
  } const accels[] = {
#define ENTRY_FULL(n, a, pt, ps) { "win." n, a, pt, ps }
#define ENTRY(n, a) ENTRY_FULL (n, a, NULL, NULL)
    ENTRY ("new-game",     "<primary>N"       ),
    ENTRY ("restart-game", "<primary>R"       ),
    ENTRY ("select-game",  "<primary>O"       ),
    ENTRY ("close-window", "<primary>W"       ),
    ENTRY ("fullscreen",   "F11"              ),
    ENTRY ("undo",         "<primary>Z"       ),
    ENTRY ("redo",         "<primary><shift>Z"),
    ENTRY ("hint",         "<primary>H"       ),
    ENTRY ("deal",         "<primary>D"       ),
    ENTRY_FULL ("help", "F1",        "s",  "'general'" ),
    ENTRY_FULL ("help", "<shift>F1", "s",  "'general'" ),
#undef ENTRY
#undef ENTRY_FULL
  };
  GtkApplication *gtk_application = GTK_APPLICATION (application);
  ArApplication *self = AR_APPLICATION (application);
  ArApplicationPrivate *priv = self->priv;
  GtkBuilder *builder;
  GError *err = NULL;
  char *path;
  guint i;

  G_APPLICATION_CLASS (ar_application_parent_class)->startup (application);

  ar_sound_enable (FALSE);
  ar_stock_init ();

  path = g_build_filename (g_get_user_config_dir (), "aisleriot", NULL);
  if (g_mkdir_with_parents (path, 0700 /* as per XDG base dir spec */) < 0 && errno != EEXIST)
    g_printerr ("Failed to create config directory \"%s\": %s\n", path, g_strerror (errno));
  g_free (path);

  path = g_build_filename (g_get_user_config_dir (), "aisleriot", "state.ini", NULL);
  priv->state_keyfile_backend = g_keyfile_settings_backend_new (path,
                                                                "/org/gnome/aisleriot/",
                                                                NULL);

  path = g_build_filename (g_get_user_config_dir (), "aisleriot", "scores.ini", NULL);
  priv->scores_keyfile_backend = g_keyfile_settings_backend_new (path,
                                                                 "/org/gnome/aisleriot/scores/",
                                                                 NULL);

  gtk_window_set_default_icon_name (priv->freecell ? "gnome-freecell" : "gnome-aisleriot");

  for (i = 0; i < G_N_ELEMENTS (accels); i++) {
    GVariant *parameter;

    if (accels[i].parameter_type) {
      parameter = g_variant_parse (G_VARIANT_TYPE (accels[i].parameter_type),
                                   accels[i].parameter_string,
                                   NULL, NULL, &err);
      g_assert_no_error (err);
    } else
      parameter = NULL;

    gtk_application_add_accelerator (gtk_application,
                                     accels[i].accel,
                                     accels[i].action_name,
                                     parameter);

  }

  g_action_map_add_action_entries (G_ACTION_MAP (self),
                                   app_entries, G_N_ELEMENTS (app_entries),
                                   self);

  builder = gtk_builder_new ();
  gtk_builder_add_from_resource (builder,
                                 "/org/gnome/aisleriot/ui/menus.ui",
                                 &err);
  g_assert_no_error (err);

  gtk_application_set_app_menu (GTK_APPLICATION (application),
                                G_MENU_MODEL (gtk_builder_get_object (builder, "appmenu")));
  g_object_unref (builder);

  priv->window = AISLERIOT_WINDOW (aisleriot_window_new (GTK_APPLICATION (application)));

  if (priv->freecell) {
    aisleriot_window_set_game_module (priv->window, FREECELL_VARIATION, NULL);
  } else {
    aisleriot_window_set_game_module (priv->window, priv->variation, NULL);
  }
}

static void
ar_application_dispose (GObject *object)
{
  ArApplication *self = AR_APPLICATION (object);
  ArApplicationPrivate *priv = self->priv;

  g_free (self->priv->variation);
  self->priv->variation = NULL;

  g_clear_object (&priv->state_keyfile_backend);
  g_clear_object (&priv->scores_keyfile_backend);

  G_OBJECT_CLASS (ar_application_parent_class)->dispose (object);
}

static void
ar_application_init (ArApplication *self)
{
  self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self,
                                            AR_TYPE_APPLICATION,
                                            ArApplicationPrivate);
}

static void
ar_application_class_init (ArApplicationClass *class)
{
  GObjectClass *object_class = G_OBJECT_CLASS (class);
  GApplicationClass *application_class = G_APPLICATION_CLASS (class);

  object_class->dispose = ar_application_dispose;

  application_class->activate = ar_application_activate;
  application_class->startup = ar_application_startup;

  g_type_class_add_private (class, sizeof (ArApplicationPrivate));
}

GtkApplication *
ar_application_new (const char *variation,
                    gboolean freecell)
{
  ArApplication *app;

  app = g_object_new (AR_TYPE_APPLICATION,
                      "application-id", "org.gnome.Aisleriot",
                      "flags", G_APPLICATION_NON_UNIQUE,
                      NULL);
  app->priv->variation = g_strdup (variation);
  app->priv->freecell = freecell != FALSE;

  return GTK_APPLICATION (app);
}

GSettings *
ar_application_state_settings_new (ArApplication *application,
                                   const char *schema)
{
  g_return_val_if_fail (AR_IS_APPLICATION (application), NULL);
  g_return_val_if_fail (schema != NULL, NULL);

  return g_settings_new_with_backend (schema,
                                      application->priv->state_keyfile_backend);
}

GSettings *
ar_application_scores_settings_new (ArApplication *application,
                                    const char *game)
{
  char *path;
  GSettings *settings;

  g_return_val_if_fail (AR_IS_APPLICATION (application), NULL);
  g_return_val_if_fail (game != NULL, NULL);

  path = g_strdup_printf ("/org/gnome/aisleriot/scores/%s/", game);
  settings = g_settings_new_with_backend_and_path (AR_SCORES_SCHEMA,
                                                   application->priv->scores_keyfile_backend,
                                                   path);
  g_free (path);

  return settings;
}

GSettings *
ar_application_options_settings_new (ArApplication *application,
                                     const char *game)
{
  char *path;
  GSettings *settings;

  g_return_val_if_fail (AR_IS_APPLICATION (application), NULL);
  g_return_val_if_fail (game != NULL, NULL);

  path = g_strdup_printf ("/org/gnome/aisleriot/options/%s/", game);
  settings = g_settings_new_with_backend_and_path (AR_OPTIONS_SCHEMA,
                                                   application->priv->state_keyfile_backend,
                                                   path);
  g_free (path);

  return settings;
}
