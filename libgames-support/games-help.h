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

#ifndef GAMES_HELP_H
#define GAMES_HELP_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

void     games_help_display      (GtkWidget *window,
                                  const char *doc_module,
                                  const char *section);
gboolean games_help_display_full (GtkWidget *window,
                                  const char *doc_module,
                                  const char *section,
                                  GError **error);

G_END_DECLS

#endif /* !GAMES_HELP_H */
