/*
 * games-sound.h: common sound player for gnome-games 
 *
 * Copyright (C) 2007 Andreas Røsdal
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 */


#ifndef GAMES_SOUND_H
#define GAMES_SOUND_H

#include <glib.h>

G_BEGIN_DECLS

void games_sound_play (gchar *filename);
void games_sound_enable (gboolean enabled);
gboolean games_sound_is_enabled (void);
GOptionGroup *games_sound_get_option_group  (void);

G_END_DECLS

#endif
