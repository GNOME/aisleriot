/*
 * Copyright © 2002,2003 Red Hat, Inc.
 *
 * This is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <glib.h>

#include "games-debug.h"

#ifdef GNOME_ENABLE_DEBUG
GamesDebugFlags _games_debug_flags;
#endif

void
_games_debug_init (void)
{
#ifdef GNOME_ENABLE_DEBUG
  const GDebugKey keys[] = {
    { "card-theme", GAMES_DEBUG_CARD_THEME },
    { "card-cache", GAMES_DEBUG_CARD_CACHE },
    { "runtime",    GAMES_DEBUG_RUNTIME    },
    { "sound ",     GAMES_DEBUG_SOUND      }
  };

  _games_debug_flags = g_parse_debug_string (g_getenv ("GAMES_DEBUG"),
                                             keys, G_N_ELEMENTS (keys));
#endif /* GNOME_ENABLE_DEBUG */
}

