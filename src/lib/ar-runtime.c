/*
 * Copyright © 2007 Andreas Røsdal <andreasr@gnome.org>
 * Copyright © 2007, 2008 Christian Persch
 * Copyright © 2009 Tor Lillqvist
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

#include <config.h>

#include <locale.h>

#ifdef G_OS_WIN32
#include <io.h>
#include <conio.h>
#define _WIN32_WINNT 0x0500
#include <windows.h>
#endif /* G_OS_WIN32 */

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "ar-debug.h"
#include "ar-profile.h"
#include "ar-show.h"

#include "ar-runtime.h"

#if defined(G_OS_WIN32) && !defined(ENABLE_BINRELOC)
#error binreloc must be enabled on win32
#endif

#if defined(ENABLE_BINRELOC) && !defined(G_OS_WIN32)

/*
 * BinReloc - a library for creating relocatable executables
 * Written by: Hongli Lai <h.lai@chello.nl>
 * http://autopackage.org/
 *
 * This source code is public domain. You can relicense this code
 * under whatever license you want.
 *
 * See http://autopackage.org/docs/binreloc/ for
 * more information and how to use this.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/** These error codes can be returned by br_init(), br_init_lib(), gbr_init() or gbr_init_lib(). */
typedef enum {
	/** Cannot allocate memory. */
	GBR_INIT_ERROR_NOMEM,
	/** Unable to open /proc/self/maps; see errno for details. */
	GBR_INIT_ERROR_OPEN_MAPS,
	/** Unable to read from /proc/self/maps; see errno for details. */
	GBR_INIT_ERROR_READ_MAPS,
	/** The file format of /proc/self/maps is invalid; kernel bug? */
	GBR_INIT_ERROR_INVALID_MAPS,
	/** BinReloc is disabled (the ENABLE_BINRELOC macro is not defined). */
	GBR_INIT_ERROR_DISABLED
} GbrInitError;

/** @internal
 * Find the canonical filename of the executable. Returns the filename
 * (which must be freed) or NULL on error. If the parameter 'error' is
 * not NULL, the error code will be stored there, if an error occured.
 */
static char *
_br_find_exe (GbrInitError *error)
{
	char *path, *path2, *line, *result;
	size_t buf_size;
	ssize_t size;
	struct stat stat_buf;
	FILE *f;

	/* Read from /proc/self/exe (symlink) */
	if (sizeof (path) > SSIZE_MAX)
		buf_size = SSIZE_MAX - 1;
	else
		buf_size = PATH_MAX - 1;
	path = (char *) g_try_malloc (buf_size);
	if (path == NULL) {
		/* Cannot allocate memory. */
		if (error)
			*error = GBR_INIT_ERROR_NOMEM;
		return NULL;
	}
	path2 = (char *) g_try_malloc (buf_size);
	if (path2 == NULL) {
		/* Cannot allocate memory. */
		if (error)
			*error = GBR_INIT_ERROR_NOMEM;
		g_free (path);
		return NULL;
	}

	strncpy (path2, "/proc/self/exe", buf_size - 1);

	while (1) {
		int i;

		size = readlink (path2, path, buf_size - 1);
		if (size == -1) {
			/* Error. */
			g_free (path2);
			break;
		}

		/* readlink() success. */
		path[size] = '\0';

		/* Check whether the symlink's target is also a symlink.
		 * We want to get the final target. */
		i = stat (path, &stat_buf);
		if (i == -1) {
			/* Error. */
			g_free (path2);
			break;
		}

		/* stat() success. */
		if (!S_ISLNK (stat_buf.st_mode)) {
			/* path is not a symlink. Done. */
			g_free (path2);
			return path;
		}

		/* path is a symlink. Continue loop and resolve this. */
		strncpy (path, path2, buf_size - 1);
	}


	/* readlink() or stat() failed; this can happen when the program is
	 * running in Valgrind 2.2. Read from /proc/self/maps as fallback. */

	buf_size = PATH_MAX + 128;
	line = (char *) g_try_realloc (path, buf_size);
	if (line == NULL) {
		/* Cannot allocate memory. */
		g_free (path);
		if (error)
			*error = GBR_INIT_ERROR_NOMEM;
		return NULL;
	}

	f = fopen ("/proc/self/maps", "r");
	if (f == NULL) {
		g_free (line);
		if (error)
			*error = GBR_INIT_ERROR_OPEN_MAPS;
		return NULL;
	}

	/* The first entry should be the executable name. */
	result = fgets (line, (int) buf_size, f);
	if (result == NULL) {
		fclose (f);
		g_free (line);
		if (error)
			*error = GBR_INIT_ERROR_READ_MAPS;
		return NULL;
	}

	/* Get rid of newline character. */
	buf_size = strlen (line);
	if (buf_size <= 0) {
		/* Huh? An empty string? */
		fclose (f);
		g_free (line);
		if (error)
			*error = GBR_INIT_ERROR_INVALID_MAPS;
		return NULL;
	}
	if (line[buf_size - 1] == 10)
		line[buf_size - 1] = 0;

	/* Extract the filename; it is always an absolute path. */
	path = strchr (line, '/');

	/* Sanity check. */
	if (strstr (line, " r-xp ") == NULL || path == NULL) {
		fclose (f);
		g_free (line);
		if (error)
			*error = GBR_INIT_ERROR_INVALID_MAPS;
		return NULL;
	}

	path = g_strdup (path);
	g_free (line);
	fclose (f);
	return path;
}

#endif /* ENABLE_BINRELOC && !G_OS_WIN32 */

static char *app_name;
static char *cached_directories[AR_RUNTIME_LAST_DIRECTORY];

typedef struct {
  ArRuntimeDirectory base_dir;
  const char *name;
} DerivedDirectory;

static const DerivedDirectory derived_directories[] = {
  /* Keep this in the same order as in the ArRuntimeDirectory enum! */
#ifdef ENABLE_BINRELOC
  { AR_RUNTIME_PREFIX,             "share"              }, /* AR_RUNTIME_DATA_DIRECTORY              */
  { AR_RUNTIME_LIBRARY_DIRECTORY,  PACKAGE              }, /* AR_RUNTIME_PKG_LIBRARY_DIRECTORY       */
  { AR_RUNTIME_DATA_DIRECTORY,     PACKAGE              }, /* AR_RUNTIME_PKG_DATA_DIRECTORY          */
#endif /* ENABLE_BINRELOC */
  { AR_RUNTIME_DATA_DIRECTORY,         "locale"         }, /* AR_RUNTIME_LOCALE_DIRECTORY            */
  { AR_RUNTIME_PKG_DATA_DIRECTORY,     "pixmaps"        }, /* AR_RUNTIME_PIXMAP_DIRECTORY     */
  { AR_RUNTIME_PKG_DATA_DIRECTORY,     "card-themes"    }, /* AR_RUNTIME_PRERENDERED_CARDS_DIRECTORY */
  { AR_RUNTIME_PKG_DATA_DIRECTORY,     "cards"          }, /* AR_RUNTIME_SCALABLE_CARDS_DIRECTORY    */
  { AR_RUNTIME_PKG_DATA_DIRECTORY,     "icons"          }, /* AR_RUNTIME_ICON_THEME_DIRECTORY        */
  { AR_RUNTIME_PKG_DATA_DIRECTORY,     "sounds"         }, /* AR_RUNTIME_SOUNDS_DIRECTORY            */
  { AR_RUNTIME_PKG_DATA_DIRECTORY,     "games"          }, /* AR_RUNTIME_GAMES_DIRECTORY             */
  { AR_RUNTIME_PKG_LIBRARY_DIRECTORY,  "games"          }, /* AR_RUNTIME_GAMES_COMPILED_DIRECTORY    */
  { AR_RUNTIME_PKG_DATA_DIRECTORY,     "help"           }, /* AR_RUNTIME_HELP_DIRECTORY              */
};

typedef int _assertion[G_N_ELEMENTS (derived_directories) + AR_RUNTIME_FIRST_DERIVED_DIRECTORY == AR_RUNTIME_LAST_DIRECTORY ? 1 : -1];

/* public API */

/**
 * ar_runtime_init:
 *
 * Initialises the runtime file localisator. This also calls setlocale,
 * and initialises gettext support and gnome-games debug support.
 *
 * NOTE: This must be called before using ANY other glib/gtk/etc function!
 * 
 * Returns: %TRUE iff initialisation succeeded
 */
gboolean
ar_runtime_init (const char *name)
{
  gboolean retval;
  const char *charset;

  setlocale (LC_ALL, "");

  if (!g_get_charset (&charset)) {
    g_printerr ("Non UTF-8 locale (%s) is not supported!\n", charset);
    return FALSE;
  }

#ifdef G_OS_WIN32
  /* On Windows, when called from a console, get console output. This works
   * only with Windows XP or higher; Windows 2000 will not have console
   * output but it will just work fine.
   */
  if (fileno (stdout) != -1 &&
      _get_osfhandle (fileno (stdout)) != -1) {
    /* stdout is fine, presumably redirected to a file or pipe.
     * Make sure stdout goes somewhere, too.
     */
    if (_get_osfhandle (fileno (stderr)) == -1) {
      dup2 (fileno (stdout), fileno (stderr));
    }
  } else {
    typedef BOOL (* WINAPI AttachConsole_t) (DWORD);

    AttachConsole_t p_AttachConsole =
      (AttachConsole_t) GetProcAddress (GetModuleHandle ("kernel32.dll"), "AttachConsole");

    if (p_AttachConsole != NULL && p_AttachConsole (ATTACH_PARENT_PROCESS)) {
      freopen ("CONOUT$", "w", stdout);
      dup2 (fileno (stdout), 1);
      freopen ("CONOUT$", "w", stderr);
      dup2 (fileno (stderr), 2);
    }
  }
#endif /* G_OS_WIN32 */

#if !GLIB_CHECK_VERSION(2, 35, 0)
  /* This also initialises gthread */
  g_type_init ();
#endif

  /* May call any glib function after this point */

  ar_profilestart ("ar_runtime_init");

  ar_debug_init ();

  app_name = g_strdup (name);

  bindtextdomain (GETTEXT_PACKAGE, ar_runtime_get_directory (AR_RUNTIME_LOCALE_DIRECTORY));
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain(GETTEXT_PACKAGE);

#ifdef ENABLE_BINRELOC
{
  const char *path;

  /* Now check that we can get the module installation directory */
  path = ar_runtime_get_directory (AR_RUNTIME_PREFIX);

  ar_debug_print (AR_DEBUG_RUNTIME,
                      "Relocation path: %s\n", path ? path : "(null)");

  retval = path != NULL;
}
#else /* !ENABLE_BINRELOC */
  retval = TRUE;
#endif /* ENABLE_BINRELOC */

  ar_profileend ("ar_runtime_init");

  return retval;
}

/**
 * ar_runtime_shutdown:
 *
 * Shuts down the runtime file localator.
 */
void
ar_runtime_shutdown (void)
{
  guint i;

  for (i = 0; i < AR_RUNTIME_LAST_DIRECTORY; ++i) {
    g_free (cached_directories[i]);
    cached_directories[i] = NULL;
  }

  g_free (app_name);
  app_name = NULL;
}

/**
 * ar_runtime_get_directory:
 * @directory:
 *
 * Returns: the path to use for @directory
 */
const char *
ar_runtime_get_directory (ArRuntimeDirectory directory)
{

  char *path = NULL;

  g_return_val_if_fail (app_name != NULL, NULL);
  g_return_val_if_fail (directory < AR_RUNTIME_LAST_DIRECTORY, NULL);

  if (cached_directories[directory])
    return cached_directories[directory];

  switch ((int) directory) {
#ifdef ENABLE_BINRELOC
    case AR_RUNTIME_PREFIX:
#ifdef G_OS_WIN32
      path = g_win32_get_package_installation_directory_of_module (NULL);
#else
      {
        GbrInitError errv = 0;
        const char *env;

        if ((env = g_getenv ("AR_RELOC_ROOT")) != NULL) {
          path = g_strdup (env);
        } else {
          char *exe, *bindir, *prefix;

          exe = _br_find_exe (&errv);
          if (exe == NULL) {
            g_printerr ("Failed to locate the binary relocation prefix (error code %u)\n", errv);
          } else {
            bindir = g_path_get_dirname (exe);
            g_free (exe);
            prefix = g_path_get_dirname (bindir);
            g_free (bindir);

            if (prefix != NULL && strcmp (prefix, ".") != 0) {
              path = prefix;
            } else {
              g_free (prefix);
            }
          }
        }
      }
#endif /* G_OS_WIN32 */
      break;
#else /* !ENABLE_BINRELOC */

    case AR_RUNTIME_PREFIX:
      path = g_strdup (PREFIX);
      break;

    case AR_RUNTIME_LIBRARY_DIRECTORY:
      path = g_strdup (LIBDIR);
      break;

    case AR_RUNTIME_DATA_DIRECTORY:
      path = g_strdup (DATADIR);
      break;

    case AR_RUNTIME_PKG_DATA_DIRECTORY:
      path = g_strdup (PKGDATADIR);
      break;

    case AR_RUNTIME_PKG_LIBRARY_DIRECTORY:
      path = g_strdup (PKGLIBDIR);
      break;

#endif /* ENABLE_BINRELOC */

    default: {
      const DerivedDirectory *base = &derived_directories[directory - AR_RUNTIME_FIRST_DERIVED_DIRECTORY];

      path = g_build_filename (ar_runtime_get_directory (base->base_dir),
                               base->name ? base->name : app_name,
                               NULL);
    }
  }

  cached_directories[directory] = path;
  return path;
}

/**
 * ar_runtime_get_file:
 * @directory:
 * @name:
 *
 * Returns: a newly allocated string containing the path to the file with name @name
 *   in the directory specicifed by @directory
 */
char *
ar_runtime_get_file (ArRuntimeDirectory directory,
                        const char *name)
{
  const char *dir;

  g_return_val_if_fail (app_name != NULL, NULL);

  dir = ar_runtime_get_directory (directory);
  if (!dir)
    return NULL;

  return g_build_filename (dir, name, NULL);
}

/**
 * ar_runtime_is_system_prefix:
 *
 * Returns: whether the runtime prefix is "/usr".
 */
gboolean
ar_runtime_is_system_prefix (void)
{
  return strcmp (ar_runtime_get_directory (AR_RUNTIME_PREFIX), "/usr") == 0;
}
