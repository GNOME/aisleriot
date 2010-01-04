/*
 * Copyright © 2002 Red Hat, Inc.
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

/* The interfaces in this file are subject to change at any time. */

#ifndef GNOME_DEBUG_H
#define GNOME_DEBUG_H

#include <glib.h>

G_BEGIN_DECLS

#define GAMES_DEBUG_LAST_RESERVED_BIT (8)

typedef enum {
  GAMES_DEBUG_BLOCKS_CACHE  = 1 << 0,
  GAMES_DEBUG_CARD_CACHE    = 1 << 1,
  GAMES_DEBUG_CARD_THEME    = 1 << 2,
  GAMES_DEBUG_RUNTIME       = 1 << 3,
  GAMES_DEBUG_SCHEME        = 1 << 4,
  GAMES_DEBUG_SOUND         = 1 << 5,
  GAMES_DEBUG_WINDOW_STATE  = 1 << 6,

  GAMES_DEBUG_GAME_DRAWING  = 1 << 7,
  GAMES_DEBUG_GAME_EVENTS   = 1 << 8,
  GAMES_DEBUG_GAME_KEYNAV   = 1 << 9,
  GAMES_DEBUG_GAME_SIZING   = 1 << 10,
  GAMES_DEBUG_GAME_STYLE    = 1 << 11
} GamesDebugFlags;

#ifdef GNOME_ENABLE_DEBUG
extern GamesDebugFlags _games_debug_flags;
#endif

void _games_debug_init (void);

static inline gboolean _games_debug_on (GamesDebugFlags flags) G_GNUC_CONST G_GNUC_UNUSED;

static inline gboolean
_games_debug_on (GamesDebugFlags flags)
{
#ifdef GNOME_ENABLE_DEBUG
  return (_games_debug_flags & flags) == flags;
#else
  return FALSE;
#endif
}

#ifdef GNOME_ENABLE_DEBUG
#define _GAMES_DEBUG_IF(flags) if (G_UNLIKELY (_games_debug_on (flags)))

#if defined(__GNUC__) && G_HAVE_GNUC_VARARGS
#define _games_debug_print(flags, fmt, ...) \
  G_STMT_START { _GAMES_DEBUG_IF(flags) g_printerr(fmt, ##__VA_ARGS__); } G_STMT_END
#else
#include <stdarg.h>
#include <glib/gstdio.h>
static void _games_debug_print (guint flags, const char *fmt, ...)
{
  if (_games_debug_on (flags)) {
    va_list  ap;
    va_start (ap, fmt);
    g_vfprintf (stderr, fmt, ap);
    va_end (ap);
  }
}
#endif

#else
#define _GAMES_DEBUG_IF(flags) if (0)
#define _games_debug_print(...)
#endif /* GNOME_ENABLE_DEBUG */

G_END_DECLS

#endif /* !GNOME_DEBUG_H */
