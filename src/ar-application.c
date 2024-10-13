/*
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
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
#include "ar-application.h"

#include <stdlib.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "ar-debug.h"
#include "ar-stock.h"
#include "ar-sound.h"
#include "ar-string-utils.h"

#include "conf.h"
#include "window.h"
#include "game.h"
#include "util.h"

struct _ArApplication {
  GtkApplication parent_instance;

  AisleriotWindow *window;
  char *variation;
  gint seed; /* unused */
  gboolean freecell; /* unused */
};


G_DEFINE_TYPE (ArApplication, ar_application, GTK_TYPE_APPLICATION);


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

  gtk_widget_destroy (GTK_WIDGET (self->window));
}

static void
ar_application_activate (GApplication *application)
{
  ArApplication *self = AR_APPLICATION (application);

  gtk_window_present (GTK_WINDOW (self->window));
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
  ArApplication *self = AR_APPLICATION (application);

  G_APPLICATION_CLASS (ar_application_parent_class)->startup (application);

  ar_sound_enable (FALSE);
  ar_stock_init ();

  gtk_window_set_default_icon_name (self->freecell ? "gnome-freecell" : "gnome-aisleriot");

  g_action_map_add_action_entries (G_ACTION_MAP (application),
                                   app_entries, G_N_ELEMENTS (app_entries),
                                   application);

  gtk_application_add_accelerator (GTK_APPLICATION (application),
                                   "F11", "app.fullscreen", NULL);
  gtk_application_add_accelerator (GTK_APPLICATION (application),
                                   "F1", "app.help", NULL);

  gtk_window_set_default_icon_name ("gnome-aisleriot");

  self->window = AISLERIOT_WINDOW (aisleriot_window_new (GTK_APPLICATION (application)));

  if (self->freecell) {
    aisleriot_window_set_game_module (self->window, FREECELL_VARIATION, NULL);
  } else {
    aisleriot_window_set_game_module (self->window, self->variation, NULL);
  }
}

static void
ar_application_dispose (GObject *object)
{
  ArApplication *self = AR_APPLICATION (object);

  g_clear_pointer (&self->variation, g_free);

  G_OBJECT_CLASS (ar_application_parent_class)->dispose (object);
}

static void
ar_application_init (ArApplication *self)
{
  /* pass */
}

static void
ar_application_class_init (ArApplicationClass *class)
{
  GObjectClass *object_class = G_OBJECT_CLASS (class);
  GApplicationClass *application_class = G_APPLICATION_CLASS (class);

  object_class->dispose = ar_application_dispose;

  application_class->activate = ar_application_activate;
  application_class->startup = ar_application_startup;
}

GtkApplication *
ar_application_new (const char *variation,
                    gboolean    freecell)
{
  ArApplication *app;

  app = g_object_new (AR_TYPE_APPLICATION,
                      "application-id", "org.gnome.aisleriot",
                      "flags", G_APPLICATION_NON_UNIQUE,
                      NULL);

  /* FIXME: This should be done as properties */
  app->variation = g_strdup (variation);
  app->freecell = freecell != FALSE;

  return GTK_APPLICATION (app);
}
