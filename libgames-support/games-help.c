/*
 *  Copyright © 2008 Thomas H.P. Andersen <phomes@gmail.com>
 *
 *  This runtime is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1, or (at your option)
 *  any later version.
 *
 *  This runtime is distributed in the hope runtime it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this runtime; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <config.h>

#include <string.h>

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#ifdef HAVE_HILDON
#include <libosso.h>

#ifdef HAVE_MAEMO_3
#include <osso-browser-interface.h>
#else
#include <tablet-browser-interface.h>
#endif /* HAVE_MAEMO_3 */
#endif /* HAVE_HILDON */

#ifdef G_OS_WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#endif /* G_OS_WIN32 */

#include "games-runtime.h"

#include "games-help.h"

static gboolean
show_uri (GdkScreen *screen,
          const char *uri,
          guint32 timestamp,
          GError **error)
{
#if 0 // def HAVE_HILDON
  osso_rpc_run_with_defaults (data->osso_context,
                              "osso_browser",
                              OSSO_BROWSER_OPEN_NEW_WINDOW_REQ,
                              NULL,
                              DBUS_TYPE_STRING, uri,
                              DBUS_TYPE_INVALID);
  return TRUE;
#else

#ifdef G_OS_WIN32
  ShellExecute (NULL, "open", uri, NULL, NULL, SW_SHOWNORMAL);
  return TRUE;
#else /* !G_OS_WIN32 */

#if GTK_CHECK_VERSION (2, 14, 0)
  return gtk_show_uri (screen, uri, timestamp, error);
#else /* GTK+ < 2.14 */
  char *argv[3] = { (char *) "xdg-open", (char *) uri, NULL };
 
  if (gdk_spawn_on_screen (screen,
                           NULL /* working directory */,
                           argv,
                           NULL /* environment */,
                           G_SPAWN_SEARCH_PATH,
                           NULL, NULL,
                           NULL,
                           error))
    return TRUE;

  g_clear_error (error);

  /* Try falling back to gnome-open */
  argv[0] = (char *) "gnome-open";
  if (gdk_spawn_on_screen (screen,
                           NULL /* working directory */,
                           argv,
                           NULL /* environment */,
                           G_SPAWN_SEARCH_PATH,
                           NULL, NULL,
                           NULL,
                           error))
    return TRUE;

  g_set_error (error, G_SPAWN_ERROR, G_SPAWN_ERROR_FAILED,
               "%s", "Failed to show help");
  return FALSE;
#endif /* GTK+ >= 2.14 */
#endif /* G_OS_WIN32 */
#endif /* HAVE_HILDON */
}

/**
 * games_help_display:
 * @window: a #GdkWindow get the #GdkScreen from, and to use
 *   as parent window for an error dialogue
 * @doc_module: the doc module name (same as DOC_MODULE from help/Makefile.am)
 * @section: a section name, or %NULL
 *
 * Opens help or displays error dialog when unable to open help.
 *
 * @window: toplevel window
 * @application: name used for the help file
 * @section: section to open, or %NULL
 *
 */
void
games_help_display (GtkWidget *window,
                    const char *doc_module,
                    const char *section)
{
  GdkScreen *screen;
  GError *error = NULL;
  char *help_uri;

  g_return_if_fail (doc_module != NULL);

  screen = gtk_widget_get_screen (GTK_WIDGET (window));

#if defined(WITH_HELP_METHOD_GHELP)
  if (section != NULL) {
    help_uri = g_strdup_printf ("ghelp:%s?%s", doc_module, section);
  } else {
    help_uri = g_strdup_printf ("ghelp:%s", doc_module);
  }
#elif defined(WITH_HELP_METHOD_FILE)
  const char *help_dir;
  const char * const *langs;
  guint i;

  langs = g_get_language_names ();
  help_dir = games_runtime_get_directory (GAMES_RUNTIME_GAME_HELP_DIRECTORY);

  help_uri = NULL;
  for (i = 0; langs[i] != NULL; ++i) {
    const char *lang = langs[i];
    char *help_file_name, *path;

    /* Filter out variants */
    if (strchr (lang, '.') != NULL ||
        strchr (lang, '@') != NULL)
      continue;

    help_file_name = g_strdup_printf ("%s." HELP_FILE_FORMAT,
                                      section ? section : doc_module);
    path = g_build_filename (help_dir,
                             lang,
                             help_file_name,
                             NULL);
    g_free (help_file_name);

    if (g_file_test (path, G_FILE_TEST_EXISTS)) {
      help_uri = g_filename_to_uri (path, NULL, NULL);
      g_free (path);
      break;
    }

    g_free (path);
  }

  if (help_uri == NULL) {
    error = g_error_new (G_IO_ERROR,
                         G_IO_ERROR_NOT_FOUND,
                         /* %s.%s is the game name + the extension HTML or XHTML, e.g. Klondike.html" */
                         _("Help file “%s.%s” not found"),
                         section ? section : doc_module,
                         HELP_FILE_FORMAT);
    goto err;
  }
    
#elif defined(WITH_HELP_METHOD_LIBRARY)
  if (section != NULL) {
    help_uri = g_strdup_printf ("http://library.gnome.org/users/%s/stable/%s.html", doc_module, section);
  } else {
    help_uri = g_strdup_printf ("http://library.gnome.org/users/%s/stable/", doc_module);
  }
#endif

  show_uri (screen, help_uri, gtk_get_current_event_time (), &error);

#if defined(WITH_HELP_METHOD_FILE)
err:
#endif

  if (error != NULL) {
    GtkWidget *dialog;
    dialog = gtk_message_dialog_new (GTK_WINDOW (window), 
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
                                     "%s", _("Unable to open help file"));

    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                              "%s", error->message);
    g_error_free (error);

#ifdef HAVE_HILDON
  /* Empty title shows up as "<unnamed>" on maemo */
  gtk_window_set_title (GTK_WINDOW (dialog), _("Error"));
#else
  gtk_window_set_title (GTK_WINDOW (dialog), "");
#endif /* HAVE_HILDON */

    g_signal_connect (dialog, "response", G_CALLBACK (gtk_widget_destroy), NULL);
    
    gtk_window_present (GTK_WINDOW (dialog));
  }

  g_free (help_uri);
}
