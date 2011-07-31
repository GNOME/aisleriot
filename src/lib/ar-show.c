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

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#ifdef G_OS_WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#endif /* G_OS_WIN32 */

#include "ar-runtime.h"

#include "ar-show.h"

/**
 * ar_show_uri:
 * @screen: screen to show the uri on or %NULL for the default screen
 * @uri: the uri to show
 * @timestamp: a timestamp to prevent focus stealing.
 * @error: a #GError that is returned in case of errors
 *
 * This is a convenience function for launching the default application
 * to show the uri.
 * Ideally the timestamp is taken from the event triggering
 * the gtk_show_uri() call, or use gtk_get_current_event_time().
 *
 * Returns: %TRUE on success, %FALSE on error.
 */
gboolean
ar_show_uri (GdkScreen *screen,
                const char *uri,
                guint32 timestamp,
                GError **error)
{
#ifdef G_OS_WIN32
  ShellExecute (NULL, "open", uri, NULL, NULL, SW_SHOWNORMAL);
  return TRUE;
#else /* !G_OS_WIN32 */

  return gtk_show_uri (screen, uri, timestamp, error);
#endif /* G_OS_WIN32 */
}

/**
 * ar_show_error:
 * @window: a transient parent window
 * @error: a #GError
 * @primary_text_format:
 * @...:
 *
 * Shows a message dialog with the given primary text, and @error's message
 * as secondary text. The dialog will be transient to @parent, and modal.
 * However, this function will *not* block until the dialogue has been dismissed.
 */
void
ar_show_error (GtkWidget *window,
                  GError *error,
                  const char *primary_text_format,
                  ...)
{
  GtkWidget *dialog;
  char *primary_text;
  va_list args;

  va_start (args, primary_text_format);
  primary_text = g_strdup_vprintf (primary_text_format, args);
  va_end (args);

  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
                                   "%s", primary_text);
  g_free (primary_text);

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                            "%s", error->message);
  gtk_window_set_title (GTK_WINDOW (dialog), "");

  g_signal_connect (dialog, "response", G_CALLBACK (gtk_widget_destroy), NULL);

  gtk_window_present (GTK_WINDOW (dialog));
}
