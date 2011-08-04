/*
 * Copyright © 2002,2003 Red Hat, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <glib.h>

#include "ar-debug.h"

#ifdef GNOME_ENABLE_DEBUG
ArDebugFlags ar_debug_flags;
#endif

void
ar_debug_init (void)
{
#ifdef GNOME_ENABLE_DEBUG
  const GDebugKey keys[] = {
    { "blocks-cache", AR_DEBUG_BLOCKS_CACHE },
    { "card-cache",   AR_DEBUG_CARD_CACHE   },
    { "card-theme",   AR_DEBUG_CARD_THEME   },
    { "runtime",      AR_DEBUG_RUNTIME      },
    { "scheme",       AR_DEBUG_SCHEME       },
    { "sound",        AR_DEBUG_SOUND        },
    { "window-state", AR_DEBUG_WINDOW_STATE },

    { "game-drawing", AR_DEBUG_GAME_DRAWING },
    { "game-events",  AR_DEBUG_GAME_EVENTS  },
    { "game-keynav",  AR_DEBUG_GAME_KEYNAV  },
    { "game-sizing",  AR_DEBUG_GAME_SIZING  },
    { "game-style",   AR_DEBUG_GAME_STYLE   }
  };
  const char *env;

  env = g_getenv ("AR_DEBUG");

  ar_debug_flags = g_parse_debug_string (env, keys, G_N_ELEMENTS (keys));
#endif /* GNOME_ENABLE_DEBUG */
}
