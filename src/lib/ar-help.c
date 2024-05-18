/*
 *  Copyright © 2008 Thomas H.P. Andersen <phomes@gmail.com>
 *  Copyright © 2007, 2008, 2009 Christian Persch
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

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "ar-show.h"
#include "ar-runtime.h"

#include "ar-help.h"

/**
 * ar_help_display_full:
 * @window: a #GdkWindow get the #GdkScreen from, and to use
 *   as parent window for an error dialogue
 * @doc_module: the doc module name (same as DOC_MODULE from help/Makefile.am)
 * @section: a section name, or %NULL
 * @error: a #GError location, or %NULL
 *
 * Opens help or returns an error.
 *
 * Returns: %TRUE on success, or %FALSE on failure with @error filled in
 */
gboolean
ar_help_display_full (GtkWidget *window,
                         const char *doc_module,
                         const char *section,
                         GError **error)
{
  GdkScreen *screen;
  char *help_uri;
  gboolean ret;

  g_return_val_if_fail (doc_module != NULL, TRUE);

  screen = gtk_widget_get_screen (GTK_WIDGET (window));

  /* FIXME: do we need to use g_uri_escape_string for doc_module and section? */

#if defined(WITH_HELP_METHOD_GHELP)
  if (section != NULL) {
    char *escaped_section;

    escaped_section = g_uri_escape_string  (section, NULL, TRUE);
    help_uri = g_strdup_printf ("help:%s/%s", doc_module, escaped_section);
    g_free (escaped_section);
  } else {
    help_uri = g_strdup_printf ("help:%s", doc_module);
  }
#elif defined(WITH_HELP_METHOD_FILE)
  const char *help_dir;
  const char * const *langs;
  guint i;

  langs = g_get_language_names ();
  help_dir = ar_runtime_get_directory (AR_RUNTIME_HELP_DIRECTORY);

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
    g_set_error (error,
                 g_quark_from_static_string ("games-help-error"), 0,
                 /* %s.%s is the game name + the extension HTML or XHTML, e.g. Klondike.html" */
                 _("Help file “%s.%s” not found"),
                 section ? section : doc_module,
                 HELP_FILE_FORMAT);
    return FALSE;
  }
    
#elif defined(WITH_HELP_METHOD_LIBRARY)
  if (section != NULL) {
    help_uri = g_strdup_printf ("https://help.gnome.org/users/%s/stable/%s.html", doc_module, section);
  } else {
    help_uri = g_strdup_printf ("https://help.gnome.org/users/%s/stable/", doc_module);
  }
#endif

  ret = ar_show_uri (screen, help_uri, gtk_get_current_event_time (), error);

  g_free (help_uri);
  return ret;
}

/**
 * ar_help_display:
 * @window: a #GdkWindow get the #GdkScreen from, and to use
 *   as parent window for an error dialogue
 * @doc_module: the doc module name (same as DOC_MODULE from help/Makefile.am)
 * @section: a section name, or %NULL
 *
 * Opens help or displays error dialog when unable to open help.
 */
void
ar_help_display (GtkWidget *window,
                    const char *doc_module,
                    const char *section)
{
  GError *error = NULL;

  if (!ar_help_display_full (window, doc_module, section, &error)) {
    ar_show_error (window, error,
                      _("Could not show help for “%s”"),
                      section ? section : g_get_application_name ());
    g_error_free (error);
  }
}
