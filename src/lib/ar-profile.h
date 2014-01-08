/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*-
 *
 * Copyright © 2005 William Jon McCann <mccann@jhu.edu>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Authors: William Jon McCann <mccann@jhu.edu>
 */

#ifndef AR_PROFILE_H
#define AR_PROFILE_H

#include <glib.h>

G_BEGIN_DECLS

#ifdef ENABLE_PROFILING
#ifdef G_HAVE_ISO_VARARGS
#define ar_profilestart(...) ar_profilelog (G_STRFUNC, "start", __VA_ARGS__)
#define ar_profileend(...)   ar_profilelog (G_STRFUNC, "end", __VA_ARGS__)
#define ar_profilemsg(...)   ar_profilelog (NULL, NULL, __VA_ARGS__)
#elif defined(G_HAVE_GNUC_VARARGS)
#define ar_profilestart(format...) ar_profilelog (G_STRFUNC, "start", format)
#define ar_profileend(format...)   ar_profilelog (G_STRFUNC, "end", format)
#define ar_profilemsg(format...)   ar_profilelog (NULL, NULL, format)
#else
#error Need either ISO or GNUC varargs macros!
#endif
#else
#define ar_profilestart(...)
#define ar_profileend(...)
#define ar_profilemsg(...)
#endif

void ar_profilelog (const char *func,
                         const char *note,
                         const char *format,
                         ...) G_GNUC_PRINTF (3, 4);

G_END_DECLS

#endif /* AR_PROFILE_H */
