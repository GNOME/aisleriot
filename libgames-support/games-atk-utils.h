/*
 *  Copyright © 2009 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef GAMES_ATK_UTILS_H
#define GAMES_ATK_UTILS_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS
  
void games_atk_util_add_atk_relation (GtkWidget *widget,
                                      GtkWidget *other,
                                      AtkRelationType type);

G_END_DECLS

#endif /* !GAMES_ATK_UTILS_H */
