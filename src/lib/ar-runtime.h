/*
 *  Copyright © 2007, 2008 Christian Persch
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef AR_RUNTIME_H
#define AR_RUNTIME_H

#include <glib.h>

G_BEGIN_DECLS

typedef enum {
  /* Base directories */
  AR_RUNTIME_PREFIX,
  AR_RUNTIME_LIBRARY_DIRECTORY,
  AR_RUNTIME_DATA_DIRECTORY,
  AR_RUNTIME_PKG_DATA_DIRECTORY,
  AR_RUNTIME_PKG_LIBRARY_DIRECTORY,

  /* Derived directories */
  AR_RUNTIME_LOCALE_DIRECTORY,
  AR_RUNTIME_PIXMAP_DIRECTORY,
  AR_RUNTIME_PRERENDERED_CARDS_DIRECTORY,
  AR_RUNTIME_SCALABLE_CARDS_DIRECTORY,
  AR_RUNTIME_ICON_THEME_DIRECTORY,
  AR_RUNTIME_SOUND_DIRECTORY,
  AR_RUNTIME_GAMES_DIRECTORY,
  AR_RUNTIME_GAMES_COMPILED_DIRECTORY,
  AR_RUNTIME_HELP_DIRECTORY, /* On win32 help is created as html with gnome-doc-tool, and put manually in this directory */

  AR_RUNTIME_LAST_DIRECTORY,
#ifdef ENABLE_BINRELOC
  AR_RUNTIME_FIRST_DERIVED_DIRECTORY = AR_RUNTIME_DATA_DIRECTORY,
#else
  AR_RUNTIME_FIRST_DERIVED_DIRECTORY = AR_RUNTIME_LOCALE_DIRECTORY,
#endif
} ArRuntimeDirectory;

gboolean        ar_runtime_init             (const char *name);
void            ar_runtime_shutdown         (void);
const char     *ar_runtime_get_directory    (ArRuntimeDirectory directory);
char           *ar_runtime_get_file         (ArRuntimeDirectory directory,
                                                const char *name);
gboolean        ar_runtime_is_system_prefix (void);

G_END_DECLS

#endif /* !AR_RUNTIME_H */
