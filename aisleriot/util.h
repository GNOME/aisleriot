/*
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef UTIL_H
#define UTIL_H

#include <glib.h>
#include <gtk/gtkwindow.h>

G_BEGIN_DECLS
  
char *aisleriot_util_get_display_filename (const char *filename);

typedef void (*AisleriotHelpFunc) (GtkWindow *, const char *, gpointer);

void aisleriot_util_set_help_func (AisleriotHelpFunc func,
                                   gpointer user_data);

void aisleriot_display_help (GtkWindow * parent, const char *game_file);

G_END_DECLS

#endif /* !UTIL_H */
