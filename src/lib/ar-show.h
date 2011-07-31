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

#ifndef AR_SHOW_H
#define AR_SHOW_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

gboolean ar_show_uri   (GdkScreen *screen,
                        const char *uri,
                        guint32 timestamp,
                        GError **error);
void     ar_show_error (GtkWidget *window,
                        GError *error,
                        const char *primary_text_format,
                        ...) G_GNUC_PRINTF (3, 4);

G_END_DECLS

#endif /* !AR_SHOW_H */
