/*
 * Copyright © 2002 Red Hat, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesse General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* The interfaces in this file are subject to change at any time. */

#ifndef GNOME_DEBUG_H
#define GNOME_DEBUG_H

#include <glib.h>

G_BEGIN_DECLS

#define AR_DEBUG_LAST_RESERVED_BIT (8)

typedef enum {
  AR_DEBUG_BLOCKS_CACHE  = 1 << 0,
  AR_DEBUG_CARD_CACHE    = 1 << 1,
  AR_DEBUG_CARD_THEME    = 1 << 2,
  AR_DEBUG_RUNTIME       = 1 << 3,
  AR_DEBUG_SCHEME        = 1 << 4,
  AR_DEBUG_SOUND         = 1 << 5,
  AR_DEBUG_WINDOW_STATE  = 1 << 6,

  AR_DEBUG_GAME_DRAWING  = 1 << 7,
  AR_DEBUG_GAME_EVENTS   = 1 << 8,
  AR_DEBUG_GAME_KEYNAV   = 1 << 9,
  AR_DEBUG_GAME_SIZING   = 1 << 10,
  AR_DEBUG_GAME_STYLE    = 1 << 11
} ArDebugFlags;

#ifdef GNOME_ENABLE_DEBUG
extern ArDebugFlags ar_debug_flags;
#endif

void ar_debug_init (void);

static inline gboolean ar_debug_on (ArDebugFlags flags) G_GNUC_CONST G_GNUC_UNUSED;

static inline gboolean
ar_debug_on (ArDebugFlags flags)
{
#ifdef GNOME_ENABLE_DEBUG
  return (ar_debug_flags & flags) == flags;
#else
  return FALSE;
#endif
}

#ifdef GNOME_ENABLE_DEBUG
#define _AR_DEBUG_IF(flags) if (G_UNLIKELY (ar_debug_on (flags)))

#if defined(__GNUC__) && G_HAVE_GNUC_VARARGS
#define ar_debug_print(flags, fmt, ...) \
  G_STMT_START { _AR_DEBUG_IF(flags) g_printerr(fmt, ##__VA_ARGS__); } G_STMT_END
#else
#include <stdarg.h>
#include <glib/gstdio.h>
static void ar_debug_print (guint flags, const char *fmt, ...)
{
  if (ar_debug_on (flags)) {
    va_list  ap;
    va_start (ap, fmt);
    g_vfprintf (stderr, fmt, ap);
    va_end (ap);
  }
}
#endif

#else
#define _AR_DEBUG_IF(flags) if (0)
#define ar_debug_print(...)
#endif /* GNOME_ENABLE_DEBUG */

G_END_DECLS

#endif /* !GNOME_DEBUG_H */
