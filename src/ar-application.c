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

#ifdef HAVE_CLUTTER
#include <cogl/cogl.h>
#include <clutter/clutter.h>
#include <clutter-gtk/clutter-gtk.h>
#endif

#include "ar-debug.h"
#include "ar-stock.h"
#include "ar-sound.h"
#include "ar-string-utils.h"

#include "conf.h"
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
};

G_DEFINE_TYPE (ArApplication, ar_application, GTK_TYPE_APPLICATION)

static void
add_main_options (ArApplication *self,
                  GOptionContext *context)
{
  const GOptionEntry aisleriot_options[] = {
    { "variation", 'v', 0, G_OPTION_ARG_STRING, &self->priv->variation,
      N_("Select the game type to play"), N_("NAME") },

    /* Ignored option, for backward compat with saved session */
    { "freecell", 0, G_OPTION_FLAG_HIDDEN, G_OPTION_ARG_NONE, &self->priv->freecell,
      NULL, NULL },
    { "seed", 's', G_OPTION_FLAG_HIDDEN, G_OPTION_ARG_STRING, &self->priv->seed,
      NULL, NULL },

    { NULL }
  };

  g_option_context_add_main_entries (context,
                                     aisleriot_options,
                                     GETTEXT_PACKAGE);
}

static void
action_new_game (GSimpleAction *action,
                 GVariant      *parameter,
                 gpointer       user_data)
{
  GtkApplication *application = user_data;
  GtkWindow *window;

  window = gtk_application_get_active_window (application);
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

static int
ar_application_command_line (GApplication *application,
                             GApplicationCommandLine *command_line)
{
  ArApplication *self = AR_APPLICATION (application);
  int argc;
  char **argv;
  int retval = 0;
  GOptionContext *context;
  GError *error = NULL;

  argv = g_application_command_line_get_arguments (command_line, &argc);

  context = g_option_context_new (NULL);

  add_main_options (self, context);

  g_option_context_set_translation_domain (context, GETTEXT_PACKAGE);
  g_option_context_add_group (context, gtk_get_option_group (TRUE));

#ifdef HAVE_CLUTTER
  g_option_context_add_group (context, cogl_get_option_group ());
  g_option_context_add_group (context, clutter_get_option_group_without_init ());
  g_option_context_add_group (context, gtk_clutter_get_option_group ());
#endif /* HAVE_CLUTTER */

  retval = g_option_context_parse (context, &argc, &argv, &error);
  g_option_context_free (context);

  if (!retval)
    {
      g_print (_("%s\nRun '%s --help' to see a full list of available command line options.\n"),
               error->message, argv[0]);
      g_error_free (error);
      return retval;
    }

  g_strfreev (argv);

  /* If we are asked for a specific game, check that it is valid. */
  if (self->priv->variation != NULL) {
    char *game_module = NULL;

    if (self->priv->variation[0] != '\0') {
      game_module = ar_filename_to_game_module (self->priv->variation);
    }

    g_free (self->priv->variation);
    self->priv->variation = game_module;
  }

  if (self->priv->variation == NULL) {
    char *pref;

    pref = ar_conf_get_string_with_default (NULL, aisleriot_conf_get_key (CONF_VARIATION), DEFAULT_VARIATION);
    self->priv->variation = ar_filename_to_game_module (pref);
    g_free (pref);
  }

  g_assert (self->priv->variation != NULL);


  aisleriot_window_set_game_module (self->priv->window, self->priv->variation, NULL);

  gtk_widget_show (GTK_WIDGET (self->priv->window));

  return retval;
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
  ArApplication *self = AR_APPLICATION (application);
  GMenu *menu;
  GMenu *section;

  G_APPLICATION_CLASS (ar_application_parent_class)->startup (application);

  aisleriot_conf_init ();
  ar_sound_enable (FALSE);
  ar_stock_init ();

  g_action_map_add_action_entries (G_ACTION_MAP (self),
                                   app_entries, G_N_ELEMENTS (app_entries),
                                   self);

  menu = g_menu_new ();

  section = g_menu_new ();
  g_menu_append (section, _("New Game"), "app.new-game");
  g_menu_append (section, _("Change Game"), "app.change-game");
  g_menu_append_section (menu, NULL, G_MENU_MODEL (section));

  section = g_menu_new ();
  g_menu_append (section, _("Statistics"), "app.statistics");
  g_menu_append (section, _("Fullscreen"), "app.fullscreen");
  g_menu_append_section (menu, NULL, G_MENU_MODEL (section));

  section = g_menu_new ();
  g_menu_append (section, _("Help"), "app.help");
  g_menu_append (section, _("About Aisleriot"), "app.about");
  g_menu_append (section, _("Quit"), "app.quit");

  g_menu_append_section (menu, NULL, G_MENU_MODEL (section));

  gtk_application_set_app_menu (GTK_APPLICATION (application),
                                G_MENU_MODEL (menu));

  gtk_application_add_accelerator (GTK_APPLICATION (application),
                                   "F11", "app.fullscreen", NULL);
  gtk_application_add_accelerator (GTK_APPLICATION (application),
                                   "F1", "app.help", NULL);

  gtk_window_set_default_icon_name ("gnome-aisleriot");

  self->priv->window = AISLERIOT_WINDOW (aisleriot_window_new (GTK_APPLICATION (application)));
}

static GObject *
ar_application_constructor (GType type,
                            guint n_construct_params,
                            GObjectConstructParam *construct_params)
{
  static GObject *self = NULL;

  if (self == NULL)
    {
      self = G_OBJECT_CLASS (ar_application_parent_class)->constructor (type,
                                                                        n_construct_params,
                                                                        construct_params);
      g_object_add_weak_pointer (self, (gpointer) &self);
      return self;
    }
  
  return g_object_ref (self);
}

static void
ar_application_dispose (GObject *object)
{
  ArApplication *self = AR_APPLICATION (object);

  G_OBJECT_CLASS (ar_application_parent_class)->dispose (object);

  g_free (self->priv->variation);
  self->priv->variation = NULL;
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

  object_class->constructor = ar_application_constructor;
  object_class->dispose = ar_application_dispose;

  application_class->activate = ar_application_activate;
  application_class->startup = ar_application_startup;
  application_class->command_line = ar_application_command_line;

  g_type_class_add_private (class, sizeof (ArApplicationPrivate));
}

GtkApplication *
ar_application_new (void)
{
  return g_object_new (AR_TYPE_APPLICATION,
                       "application-id", "org.gnome.Aisleriot",
                       "flags", G_APPLICATION_NON_UNIQUE |
                                G_APPLICATION_HANDLES_COMMAND_LINE,
                       NULL);
}
