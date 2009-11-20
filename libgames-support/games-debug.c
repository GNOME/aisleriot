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
    { "blocks-cache", GAMES_DEBUG_BLOCKS_CACHE },
    { "card-cache",   GAMES_DEBUG_CARD_CACHE   },
    { "card-theme",   GAMES_DEBUG_CARD_THEME   },
    { "runtime",      GAMES_DEBUG_RUNTIME      },
    { "scheme",       GAMES_DEBUG_SCHEME       },
    { "sound",        GAMES_DEBUG_SOUND        },
    { "window-state", GAMES_DEBUG_WINDOW_STATE }
  };
  const char *env;

  env = g_getenv ("GAMES_DEBUG");

#if !GLIB_CHECK_VERSION (2, 16, 0)
  /* g_parse_debug_string is only NULL-safe since 2.16 */
  if (env == NULL)
    return;
#endif

  _games_debug_flags = g_parse_debug_string (env, keys, G_N_ELEMENTS (keys));
#endif /* GNOME_ENABLE_DEBUG */
}
