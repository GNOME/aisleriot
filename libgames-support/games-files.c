/* games-files.c
   Copyright © 2003 Callum McKenzie

   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published 
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

/* The API in this file is best described as "raw and wriggling". They
 * are all meant ot be specialised routines to collect lists of files
 * that a game might be interested in. I'm writing them as I perceive
 * and so some of them should be replaced or removed eventually. */

#include <config.h>

#include <string.h>
#include <stdarg.h>

#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#ifdef G_OS_WIN32
#define STRICT
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#ifndef S_IWUSR
# define S_IWUSR _S_IWRITE
#endif
#endif

#include "games-files.h"
#include "games-card-common.h"

G_DEFINE_TYPE (GamesFileList, games_file_list, G_TYPE_OBJECT)

/* Remove duplicate names form the list */
     static void
       games_file_list_remove_duplicates (GamesFileList * filelist)
{
  GList *l;

  if (filelist == NULL)
    return;

  l = filelist->list;

  if ((l == NULL) || (l->next == NULL))
    return;

  while (l && l->next) {
    if (g_utf8_collate (l->data, l->next->data) == 0) {
      g_free (l->next->data);
      l = g_list_delete_link (l, l->next);
    }
    l = l->next;
  }
}

static GList *
games_file_list_new_internal (const gchar * glob, va_list path)
{
  GPatternSpec *filespec = g_pattern_spec_new (glob);
  gchar *pathelement;
  GList *list = NULL;
  GDir *dir;
  const gchar *filename;
  gchar *fullname;

  while ((pathelement = va_arg (path, gchar *)) != NULL) {
    dir = g_dir_open (pathelement, 0, NULL);
    if (dir != NULL) {
      while ((filename = g_dir_read_name (dir)) != NULL) {
	if (g_pattern_match_string (filespec, filename)) {
	  fullname = g_build_filename (pathelement, filename, NULL);
	  if (g_file_test (fullname, G_FILE_TEST_IS_REGULAR)) {
	    list = g_list_append (list, fullname);
	  } else
	    g_free (fullname);
	}
      }
      g_dir_close (dir);
    }
  }

  g_pattern_spec_free (filespec);

  return list;
}

/**
 * games_file_list_new:
 * @glob: A pattern to match files against. See g_pattern_spec_new () for 
 * details.
 * @varargs: A NULL terminated list of strings containing directory names to 
 * be searched for files.
 * 
 * This function takes a glob and a NULL terminated list of directories 
 * and constructs a list of all files in the directories that match the glob. 
 * Only regular files are returned.
 * 
 * Return value: A pointer to a new GamesFileList containing files 
 * matching the glob in the path.
 **/
GamesFileList *
games_file_list_new (const gchar * glob, ...)
{
  GamesFileList *filelist;
  va_list paths;

  filelist = g_object_new (GAMES_FILE_LIST_TYPE, NULL);

  va_start (paths, glob);
  filelist->list = games_file_list_new_internal (glob, paths);
  va_end (paths);

  filelist->list =
    g_list_sort (filelist->list, (GCompareFunc) g_utf8_collate);
  games_file_list_remove_duplicates (filelist);

  return filelist;
}

/* Transform the list of files to be only the basenames. */
void
games_file_list_transform_basename (GamesFileList * filelist)
{
  GList *current = filelist->list;
  gchar *shortname;

  while (current) {
    shortname = g_path_get_basename ((gchar *) current->data);
    g_free (current->data);
    current->data = (gpointer) shortname;
    current = g_list_next (current);
  }

  games_file_list_remove_duplicates (filelist);
}

static GSList *image_suffix_list = NULL;
static GStaticMutex image_suffix_mutex = G_STATIC_MUTEX_INIT;

/* We only want to initilise the list of suffixes once, this is
 * the function that does it. It might even be thread safe, not that
 * this has been tested ... */
static void
games_image_suffix_list_init (void)
{
  GSList *pixbuf_formats;
  GSList *element;
  GdkPixbufFormat *formats;
  gchar **suffices;
  gchar **suffix;

  g_static_mutex_lock (&image_suffix_mutex);

  /* This check needs to be inside the lock to make sure that another
   * thread haasn't half-completed the list. */
  if (image_suffix_list) {
    g_static_mutex_unlock (&image_suffix_mutex);
    return;
  }

  pixbuf_formats = gdk_pixbuf_get_formats ();

  /* Search through the list of formats for the suffices. */
  element = pixbuf_formats;
  while (element) {
    formats = element->data;
    suffices = gdk_pixbuf_format_get_extensions (formats);

    suffix = suffices;
    while (*suffix) {
      image_suffix_list =
	g_slist_append (image_suffix_list, g_strdup_printf (".%s", *suffix));
      suffix++;
    }

    g_strfreev (suffices);

    element = g_slist_next (element);
  }

  pixbuf_formats = gdk_pixbuf_get_formats ();

  g_slist_free (pixbuf_formats);

  g_static_mutex_unlock (&image_suffix_mutex);
}

static GList *
games_file_list_new_images_single (gchar * directory)
{
  GDir *dir;
  GList *list = NULL;
  const gchar *filename;
  gchar *fullname;
  GSList *suffix;

  dir = g_dir_open (directory, 0, NULL);
  if (!dir)
    return NULL;

  games_image_suffix_list_init ();

  while ((filename = g_dir_read_name (dir)) != NULL) {
    suffix = image_suffix_list;
    while (suffix) {
      if (g_str_has_suffix (filename, suffix->data)) {
	fullname = g_build_filename (directory, filename, NULL);
	if (g_file_test (fullname, G_FILE_TEST_IS_REGULAR)) {
	  list = g_list_append (list, fullname);
	} else
	  g_free (fullname);
	break;
      }
      suffix = g_slist_next (suffix);
    }
  }

  g_dir_close (dir);

  return list;
}

/**
 * games_file_list_new_images:
 * @path1: A NULL-terminated list of strings containing directories to be 
 * searched.
 * 
 * A convenience function which constructs a list of filenames which
 * are images that can be loaded via gdk-pixbuf. Whether a file is an
 * image or not is determined by its extension. The list of possible
 * extensions is determined by querying the gdk-pixbuf library the
 * first time this function is called.
 * 
 * Return value: A new GamesFileList containing the list of image files.
 **/
GamesFileList *
games_file_list_new_images (gchar * path1, ...)
{
  GamesFileList *filelist;
  GList *list;
  gchar *pathentry;
  va_list paths;

  filelist = g_object_new (GAMES_FILE_LIST_TYPE, NULL);

  filelist->list = games_file_list_new_images_single (path1);
  va_start (paths, path1);
  while ((pathentry = va_arg (paths, gchar *)) != NULL) {
    list = g_list_concat (filelist->list,
			  games_file_list_new_images_single (pathentry));
  }
  va_end (paths);

  filelist->list =
    g_list_sort (filelist->list, (GCompareFunc) g_utf8_collate);
  games_file_list_remove_duplicates (filelist);

  return filelist;
}

/**
 * games_file_list_create_widget:
 * @gamesfilelist: The list of files to use.
 * @selection: The name to select as the default. NULL means no default.
 * @flags: A set of flags to specify how the names are displayed. 
 * 
 * Create a combo box with the given list of strings as the entries. If 
 * selection is non-NULL the matching file name is selected by default. 
 * Otherwise nothing is selected. The flags affect how the names are 
 * displayed. The valid flags are GAMES_FILE_LIST_REMOVE_EXTENSION, which 
 * removes extensions, and GAMES_FILE_LIST_REPLACE_UNDERSCORES with replaces
 * underscores with spaces.
 * 
 * Return value: A widget with the list of names.
 **/
GtkWidget *
games_file_list_create_widget (GamesFileList * gamesfilelist,
			       const gchar * selection, guint flags)
{
  gint itemno;
  GtkComboBox *widget;
  gchar *visible, *string;
  GList *filelist = gamesfilelist->list;
  gboolean found = FALSE;

  widget = GTK_COMBO_BOX (gtk_combo_box_new_text ());

  itemno = 0;
  while (filelist) {
    gchar *s;

    string = (gchar *) filelist->data;
    visible = g_strdup (string);

    /* These are a bit hackish, but we don't yet have a good regexp
     * library in glib. There are probably some ways these could
     * seriously mangle unicode strings. */
    if (flags & GAMES_FILE_LIST_REMOVE_EXTENSION) {
      s = g_strrstr (visible, ".");
      if (s)
	*s = '\0';
    }
    if (flags & GAMES_FILE_LIST_REPLACE_UNDERSCORES) {
      s = visible;
      while (*s) {
	if (*s == '_')
	  *s = ' ';
	s++;
      }
    }

    gtk_combo_box_append_text (widget, visible);
    if (selection && (!strcmp (string, selection))) {
      gtk_combo_box_set_active (widget, itemno);
      found = TRUE;
    }

    g_free (visible);

    itemno++;
    filelist = g_list_next (filelist);
  }
  if (!found)
    gtk_combo_box_set_active (widget, 0);

  return GTK_WIDGET (widget);
}

/**
 * games_file_list_for_each:
 * @filelist: The file list to iterate over.
 * @function: The function to call on each item. It gets called with two 
 * arguments: the file name and the pointer supplied to this function in 
 * the userdata argument.
 * @userdata: An arbitrary pointer that gets passed as the second argument
 * to each call of function.
 * 
 * Apply a function to each file name in the list.
 **/
void
games_file_list_for_each (GamesFileList * filelist, GFunc function,
			  gpointer userdata)
{
  g_list_foreach (filelist->list, function, userdata);
}

/**
 * games_file_list_find:
 * @filelist: The file list to iterate over.
 * @function: The function to call on each item. It gets called with two 
 * arguments: the file name and the pointer supplied to this function in 
 * the userdata argument.
 * @userdata: An arbitrary pointer that gets passed as the second argument
 * to each call of function.
 * 
 * Find a file name by iterating through a list until the given function
 * returns 0.
 *
 * Return value: A newly allocated string containing a copy of the file name,
 * or NULL if no file name was found.
 **/
gchar *
games_file_list_find (GamesFileList * filelist, GCompareFunc function,
		      gpointer userdata)
{
  GList *element;

  element = g_list_find_custom (filelist->list, userdata, function);

  return element ? g_strdup ((gchar *) element->data) : NULL;
}

/**
 * games_file_list_get_nth:
 * @filelist: The list of file names to select from.
 * @n: The 0-based index into the list.
 * 
 * Obtain the (n+1)th file name from the list.
 * 
 * Return value: 
 **/
/* Return the nth filename in the list. */
gchar *
games_file_list_get_nth (GamesFileList * filelist, gint n)
{
  return (gchar *) g_list_nth_data (filelist->list, n);
}

static void
games_file_list_finalize (GObject * object)
{
  GamesFileList *filelist = GAMES_FILE_LIST (object);

  /* For simplicity we haven't used the dispose method since we can
   * guarantee that everything this references doesn't reference itself. */

  g_list_foreach (filelist->list, (GFunc) g_free, NULL);
  g_list_free (filelist->list);

  G_OBJECT_CLASS (games_file_list_parent_class)->finalize (object);
}

static void
games_file_list_class_init (GamesFileListClass * class)
{
  GObjectClass *oclass = G_OBJECT_CLASS (class);

  oclass->finalize = games_file_list_finalize;
}

static void
games_file_list_init (GamesFileList * filelist)
{
}

/**
 * games_file_list_card_themes:
 * @scalable: whether to look for scalable or prerendered themes
 *
 * Note that the returned #GamesFileList's list contains the found
 * theme names in the filename encoding!
 * 
 * Returns: a new #GamesFileList containing the found themes
 */
GamesFileList *
games_file_list_card_themes (gboolean scalable)
{
  GamesFileList *files;
  GList *l;
  const char *glob, *ext, *dir;

#ifdef HAVE_RSVG
  if (scalable) {
    glob = "*.svg";
    ext = ".svg";
    dir = SCALABLE_CARDS_DIR;
  } else
#endif /* HAVE_RSVG */
  {
    glob = "*.card-theme";
    ext = ".card-theme";
    dir = PRERENDERED_CARDS_DIR;
  }

  files = games_file_list_new (glob, dir, NULL);
  games_file_list_transform_basename (files);

  for (l = files->list; l != NULL; l = l->next) {
    const char *filename = (const char *) l->data;
    char *dot;

    dot = g_strrstr (filename, ext);
    if (dot) {
      *dot = '\0';
    }
  }

  return files;
}

#if defined (G_OS_WIN32) && defined (PREFIX)

static const gchar *
games_toplevel_directory (void)
{
  static gchar *toplevel = NULL;

  if (toplevel)
    return toplevel;

  {
    gchar *filename;
    gchar *sep1, *sep2;

    if (G_WIN32_HAVE_WIDECHAR_API ()) {
      wchar_t w_filename[MAX_PATH];

      if (GetModuleFileNameW (NULL,
			      w_filename, G_N_ELEMENTS (w_filename)) == 0)
	g_error ("GetModuleFilenameW failed");

      filename = g_utf16_to_utf8 (w_filename, -1, NULL, NULL, NULL);
      if (filename == NULL)
	g_error ("Converting module filename to UTF-8 failed");
    } else {
      gchar cp_filename[MAX_PATH];

      if (GetModuleFileNameA (NULL,
			      cp_filename, G_N_ELEMENTS (cp_filename)) == 0)
	g_error ("GetModuleFilenameA failed");

      filename = g_locale_to_utf8 (cp_filename, -1, NULL, NULL, NULL);
      if (filename == NULL)
	g_error ("Converting module filename to UTF-8 failed");
    }

    /* If the executable file name is of the format
     * <foobar>\bin\*.exe , use <foobar>.
     * Otherwise, use the directory where the executable is.
     */

    sep1 = strrchr (filename, '\\');
    *sep1 = '\0';

    sep2 = strrchr (filename, '\\');
    if (sep2 != NULL) {
      *sep2 = '\0';
    }

    toplevel = filename;
  }

  return toplevel;
}
#endif

/**
 * games_path_runtime_fix_private:
 * @path: A pointer to a string (allocated with g_malloc) that is
 *        (or could be) a pathname.
 *
 * On Windows, this function checks if the string pointed to by @path
 * starts with the compile-time prefix, and in that case, replaces the
 * prefix with the run-time one.  @path should be a pointer to a
 * dynamically allocated (with g_malloc, g_strconcat, etc) string. If
 * the replacement takes place, the original string is deallocated,
 * and *@path is replaced with a pointer to a new string with the
 * run-time prefix spliced in.
 *
 * On Linux, it does the same thing, but only if BinReloc support is enabled.
 * On other Unices, it does nothing because those platforms don't have a
 * way to find out where our binary is.
 */
static void
games_path_runtime_fix_private (gchar **path)
{
#if defined (G_OS_WIN32) && defined (PREFIX)
  gchar *p;

      /* This is a compile-time entry. Replace the path with the
       * real one on this machine.
       */
      p = *path;
      *path = g_strconcat (games_toplevel_directory (),
                           "\\",
                           *path + strlen (PREFIX "/"),
                           NULL);
      g_free (p);
  /* Replace forward slashes with backslashes, just for
   * completeness */
  p = *path;
  while ((p = strchr (p, '/')) != NULL)
    {
      *p = '\\';
      p++;
    }
#endif
}


gchar *
games_path_runtime_fix (const gchar *path)
{
  gchar *p = g_strdup (path);
  games_path_runtime_fix_private (&p);

  return p;
}


gchar *
games_build_filename (const gchar * path, const gchar * filename)
{
  gchar *p = g_strdup (path);
	  
  games_path_runtime_fix_private (&p);

  gchar *result = g_build_filename (p, filename, NULL);
  g_free (p);

  return result;
}
