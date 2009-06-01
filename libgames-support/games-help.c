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

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "games-help.h"

/**
 * games_help_display:
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
                    const char *app_name,
                    const char *section)
{
#if GTK_CHECK_VERSION (2, 14, 0)
  GdkScreen *screen;
  GError *error = NULL;
  char *help_string;

  screen = gtk_widget_get_screen (GTK_WIDGET (window));

  if (section) {
    help_string = g_strconcat ("ghelp:", app_name, "?", section, NULL);
  } else {
    help_string = g_strconcat ("ghelp:", app_name, NULL);
  }

  gtk_show_uri (screen, help_string, gtk_get_current_event_time (), &error);

  if (error != NULL) {
    GtkWidget *d;
    d = gtk_message_dialog_new (GTK_WINDOW (window), 
                              GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                              GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, 
                              "%s", _("Unable to open help file"));
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (d),
                              "%s", error->message);
    g_signal_connect (d, "response", G_CALLBACK (gtk_widget_destroy), NULL);
    gtk_window_present (GTK_WINDOW (d));

    g_error_free (error);
  }

  g_free(help_string);

#else /* GTK+ < 2.14 */
#error FIXME: games_help_display unimplemented on hildon!
#endif /* GTK+ >= 2.14 */
}
