/*
 * Copyright © 2007 Andreas Røsdal <andreasr@gnome.org>
 * Copyright © 2007, 2008 Christian Persch
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This runtime is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this runtime; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#include <config.h>

#if defined (G_OS_WIN32)
#include <windows.h>
#include <io.h>
#define HELP_EXT "xhtml"
/* FIXME On win32 help is created as html with gnome-doc-tool,
 * and put manually in the directory below.
 */
#define HELPDIR PKGDATADIR "/aisleriot/help"
#endif /* G_OS_WIN32 */

#include "games-runtime.h"

static char *app_name;
static char *cached_directories[GAMES_RUNTIME_LAST_DIRECTORY];
#ifdef G_OS_WIN32
static char *module_path;
#endif

typedef struct {
  GamesRuntimeDirectory base_dir;
  const char *name;
} DerivedDirectory;

static const DerivedDirectory derived_directories[] = {
  /* Keep this in the same order as in the GamesRuntimeDirectory enum! */
#ifdef G_OS_WIN32
  { GAMES_RUNTIME_MODULE_DIRECTORY,   "share"              }, /* GAMES_RUNTIME_DATA_DIRECTORY              */
  { GAMES_RUNTIME_DATA_DIRECTORY,     "gnome-games-common" }, /* GAMES_RUNTIME_COMMON_DATA_DIRECTORY       */
  { GAMES_RUNTIME_DATA_DIRECTORY,     "gnome-games"        }, /* GAMES_RUNTIME_PKG_DATA_DIRECTORY          */
#endif /* G_OS_WIN32 */
  { GAMES_RUNTIME_DATA_DIRECTORY,         "locale"         }, /* GAMES_RUNTIME_LOCALE_DIRECTORY            */
  { GAMES_RUNTIME_COMMON_DATA_DIRECTORY,  "pixmaps"        }, /* GAMES_RUNTIME_COMMON_PIXMAP_DIRECTORY     */
  { GAMES_RUNTIME_COMMON_DATA_DIRECTORY,  "card-themes"    }, /* GAMES_RUNTIME_PRERENDERED_CARDS_DIRECTORY */
  { GAMES_RUNTIME_COMMON_DATA_DIRECTORY,  "cards"          }, /* GAMES_RUNTIME_SCALABLE_CARDS_DIRECTORY    */
  { GAMES_RUNTIME_PKG_DATA_DIRECTORY,     "icons"          }, /* GAMES_RUNTIME_ICON_THEME_DIRECTORY        */
  { GAMES_RUNTIME_PKG_DATA_DIRECTORY,     "pixmaps"        }, /* GAMES_RUNTIME_PIXMAP_DIRECTORY            */
  { GAMES_RUNTIME_PKG_DATA_DIRECTORY,     "sounds"         }, /* GAMES_RUNTIME_SOUNDS_DIRECTORY            */
  { GAMES_RUNTIME_PKG_DATA_DIRECTORY,     NULL             }, /* GAMES_RUNTIME_GAME_DATA_DIRECTORY         */
  { GAMES_RUNTIME_GAME_DATA_DIRECTORY,    "games"          }, /* GAMES_RUNTIME_GAME_GAMES_DIRECTORY        */
  { GAMES_RUNTIME_GAME_DATA_DIRECTORY,    "pixmaps"        }, /* GAMES_RUNTIME_GAME_PIXMAP_DIRECTORY       */
  { GAMES_RUNTIME_GAME_DATA_DIRECTORY,    "themes"         }, /* GAMES_RUNTIME_GAME_THEME_DIRECTORY        */
  { GAMES_RUNTIME_GAME_DATA_DIRECTORY,    "help"           }, /* GAMES_RUNTIME_GAME_HELP_DIRECTORY         */
};

typedef int _assertion[G_N_ELEMENTS (derived_directories) + GAMES_RUNTIME_FIRST_DERIVED_DIRECTORY == GAMES_RUNTIME_LAST_DIRECTORY ? 1 : -1];

/* public API */

/**
 * games_runtime_init:
 *
 * Initialises the runtime file localisator.
 * 
 * Returns: %TRUE iff initialisation succeeded
 */
gboolean
games_runtime_init (const char *name)
{
  app_name = g_strdup (name);

#ifdef G_OS_WIN32
  return games_runtime_get_directory (GAMES_RUNTIME_MODULE_DIRECTORY) != NULL;
#else
  return TRUE;
#endif
}

/**
 * games_runtime_shutdown:
 *
 * Shuts down the runtime file localator.
 */
void
games_runtime_shutdown (void)
{
  guint i;

  for (i = 0; i < GAMES_RUNTIME_LAST_DIRECTORY; ++i) {
    g_free (cached_directories[i]);
    cached_directories[i] = NULL;
  }

  g_free (app_name);
  app_name = NULL;
}

/**
 * games_runtime_get_directory:
 * @runtime: the #GamesProgram instance
 * @directory:
 *
 * Returns: the path to use for @directory
 */
const char *
games_runtime_get_directory (GamesRuntimeDirectory directory)
{

  char *path = NULL;

  g_return_val_if_fail (app_name != NULL, NULL);
  g_return_val_if_fail (directory >= 0 && directory < GAMES_RUNTIME_LAST_DIRECTORY, NULL);

  if (cached_directories[directory])
    return cached_directories[directory];

  switch (directory) {
#ifndef G_OS_WIN32
    case GAMES_RUNTIME_DATA_DIRECTORY:
      path = g_strdup (DATADIR);
      break;

    case GAMES_RUNTIME_COMMON_DATA_DIRECTORY:
      path = g_build_filename (DATADIR, "gnome-games-common", NULL);
      break;

    case GAMES_RUNTIME_PKG_DATA_DIRECTORY:
      path = g_strdup (PKGDATADIR);
      break;

#else /* G_OS_WIN32 */
    case GAMES_RUNTIME_MODULE_DIRECTORY:
      path = g_win32_get_package_installation_directory_of_module (NULL);
      break;
#endif /* !G_OS_WIN32 */

    default: {
      const DerivedDirectory *base = &derived_directories[directory - GAMES_RUNTIME_FIRST_DERIVED_DIRECTORY];

      path = g_build_filename (games_runtime_get_directory (base->base_dir),
                               base->name ? base->name : app_name,
                               NULL);
    }
  }

  cached_directories[directory] = path;
  return path;
}

/**
 * games_runtime_get_file:
 * @runtime: the #GamesProgram instance
 * @directory:
 * @name:
 *
 * Returns: a newly allocated string containing the path to the file with name @name
 *   in the directory specicifed by @directory
 */
char *
games_runtime_get_file (GamesRuntimeDirectory directory,
                        const char *name)
{
  const char *dir;

  g_return_val_if_fail (app_name != NULL, NULL);

  dir = games_runtime_get_directory (directory);
  if (!dir)
    return NULL;

  return g_build_filename (dir, name, NULL);
}
