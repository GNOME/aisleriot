/*
 *  Copyright © 2007, 2008 Christian Persch
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

#ifndef AR_RUNTIME_H
#define AR_RUNTIME_H

#include <glib.h>

#ifdef HAVE_HILDON
#include <libosso.h>
#endif

G_BEGIN_DECLS

typedef enum {
  /* Base directories */
  AR_RUNTIME_PREFIX,
  AR_RUNTIME_DATA_DIRECTORY,
  AR_RUNTIME_COMMON_DATA_DIRECTORY,
  AR_RUNTIME_PKG_DATA_DIRECTORY,
  AR_RUNTIME_SCORES_DIRECTORY,

  /* Derived directories */
  AR_RUNTIME_LOCALE_DIRECTORY,

  AR_RUNTIME_COMMON_PIXMAP_DIRECTORY,
  AR_RUNTIME_PRERENDERED_CARDS_DIRECTORY,
  AR_RUNTIME_SCALABLE_CARDS_DIRECTORY,

  AR_RUNTIME_ICON_THEME_DIRECTORY,
  AR_RUNTIME_PIXMAP_DIRECTORY,
  AR_RUNTIME_SOUND_DIRECTORY,

  AR_RUNTIME_GAME_DATA_DIRECTORY,
  AR_RUNTIME_GAME_GAMES_DIRECTORY,
  AR_RUNTIME_GAME_PIXMAP_DIRECTORY,
  AR_RUNTIME_GAME_THEME_DIRECTORY,
  /* FIXME On hildon and win32 help is created as html with gnome-doc-tool, and put manually in this directory */
  AR_RUNTIME_GAME_HELP_DIRECTORY,

  AR_RUNTIME_LAST_DIRECTORY,
#ifdef ENABLE_BINRELOC
  AR_RUNTIME_FIRST_DERIVED_DIRECTORY = AR_RUNTIME_DATA_DIRECTORY,
#else
  AR_RUNTIME_FIRST_DERIVED_DIRECTORY = AR_RUNTIME_LOCALE_DIRECTORY,
#endif
} ArRuntimeDirectory;

gboolean        ar_runtime_init             (const char *name);
#ifdef HAVE_HILDON
gboolean        ar_runtime_init_with_osso   (const char *name,
                                                const char *service_name);
osso_context_t* ar_runtime_get_osso_context (void);
#endif /* HAVE_HILDON */
void            ar_runtime_shutdown         (void);
const char     *ar_runtime_get_directory    (ArRuntimeDirectory directory);
char           *ar_runtime_get_file         (ArRuntimeDirectory directory,
                                                const char *name);
int             ar_runtime_get_gpl_version  (void);
gboolean        ar_runtime_is_system_prefix (void);

G_END_DECLS

#endif /* !AR_RUNTIME_H */
