/*
 *  Copyright © 2009 Thomas H.P. Andersen <phomes@gmail.com>
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

#ifndef GAMES_GLIB_COMPAT_H
#define GAMES_GLIB_COMPAT_H

#include <glib.h>
#include <glib-object.h>

G_BEGIN_DECLS

#if !GLIB_CHECK_VERSION (2, 10, 0)

#define g_slice_new(T) g_new (T, 1)
#define g_slice_new0(T) g_new0 (T, 1)
#define g_slice_free(T,ptr) g_free (ptr)

#endif /* GLIB < 2.10 */

#if !GLIB_CHECK_VERSION (2, 14, 0)

#define G_PARAM_STATIC_STRINGS (G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB)

#define g_timeout_add_seconds(timeout,callback,data) g_timeout_add ((timeout)*1000, callback, data)
#define gdk_threads_add_timeout_seconds(timeout,callback,data) g_timeout_add ((timeout)*1000, callback, data)
#endif /* GLIB < 2.14 */

G_END_DECLS

#endif /* !GAMES_GLIB_COMPAT_H */
