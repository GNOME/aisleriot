/*
 *  Copyright © 2008 Thomas H.P. Andersen <phomes@gmail.com>
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

#ifndef AR_HELP_H
#define AR_HELP_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

void     ar_help_display      (GtkWidget *window,
                                  const char *doc_module,
                                  const char *section);
gboolean ar_help_display_full (GtkWidget *window,
                                  const char *doc_module,
                                  const char *section,
                                  GError **error);

G_END_DECLS

#endif /* !AR_HELP_H */
