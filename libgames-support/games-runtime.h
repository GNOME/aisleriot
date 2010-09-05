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

#ifndef GAMES_RUNTIME_H
#define GAMES_RUNTIME_H

#include <glib.h>

#ifdef HAVE_HILDON
#include <libosso.h>
#endif

G_BEGIN_DECLS

typedef enum {
  /* Base directories */
  GAMES_RUNTIME_PREFIX,
  GAMES_RUNTIME_DATA_DIRECTORY,
  GAMES_RUNTIME_COMMON_DATA_DIRECTORY,
  GAMES_RUNTIME_PKG_DATA_DIRECTORY,
  GAMES_RUNTIME_SCORES_DIRECTORY,

  /* Derived directories */
  GAMES_RUNTIME_LOCALE_DIRECTORY,

  GAMES_RUNTIME_COMMON_PIXMAP_DIRECTORY,
  GAMES_RUNTIME_PRERENDERED_CARDS_DIRECTORY,
  GAMES_RUNTIME_SCALABLE_CARDS_DIRECTORY,

  GAMES_RUNTIME_ICON_THEME_DIRECTORY,
  GAMES_RUNTIME_PIXMAP_DIRECTORY,
  GAMES_RUNTIME_SOUND_DIRECTORY,

  GAMES_RUNTIME_GAME_DATA_DIRECTORY,
  GAMES_RUNTIME_GAME_GAMES_DIRECTORY,
  GAMES_RUNTIME_GAME_PIXMAP_DIRECTORY,
  GAMES_RUNTIME_GAME_THEME_DIRECTORY,
  /* FIXME On hildon and win32 help is created as html with gnome-doc-tool, and put manually in this directory */
  GAMES_RUNTIME_GAME_HELP_DIRECTORY,

  GAMES_RUNTIME_LAST_DIRECTORY,
#ifdef ENABLE_BINRELOC
  GAMES_RUNTIME_FIRST_DERIVED_DIRECTORY = GAMES_RUNTIME_DATA_DIRECTORY,
#else
  GAMES_RUNTIME_FIRST_DERIVED_DIRECTORY = GAMES_RUNTIME_LOCALE_DIRECTORY,
#endif
} GamesRuntimeDirectory;

gboolean games_runtime_init (const char *name);

#ifdef HAVE_HILDON

gboolean games_runtime_init_with_osso (const char *name,
                                       const char *service_name);

osso_context_t* games_runtime_get_osso_context (void);

#endif /* HAVE_HILDON */

void games_runtime_shutdown (void);

const char * games_runtime_get_directory (GamesRuntimeDirectory directory);

char * games_runtime_get_file (GamesRuntimeDirectory directory,
                               const char *name);

int games_runtime_get_gpl_version (void);

gboolean games_runtime_is_system_prefix (void);

G_END_DECLS

#endif /* !GAMES_RUNTIME_H */
