/*
 * Copyright © 2007 Christian Persch
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#ifndef AISLERIOT_GUILE16_COMPAT_H
#define AISLERIOT_GUILE16_COMPAT_H

#include <string.h>

#ifdef HAVE_GUILE_1_8
#error Don't include this file on guile 1.8
#endif

#define scm_to_locale_string(object) (strdup (SCM_STRING_CHARS (object)))
#define scm_from_locale_string(string) (scm_makfrom0str (string))
#define scm_is_string(object) (SCM_STRINGP (object))
#define scm_is_false(object) (!SCM_NFALSEP (object))
#define scm_to_int(object) (SCM_INUM (object))
#define scm_to_uint(object) (SCM_INUM (object))
#define scm_from_int(object) (SCM_MAKINUM (object))
#define scm_from_uint(object) (SCM_MAKINUM (object))
#define scm_from_uint32(object) (SCM_MAKINUM (object))
#define scm_from_locale_symbol(string) (scm_str2symbol (string))

#endif /* !AISLERIOT_GUILE16_COMPAT_H */
