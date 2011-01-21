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

#include "games-runtime.h"

#include "games-files.h"

G_DEFINE_TYPE (GamesFileList, games_file_list, G_TYPE_OBJECT)

struct GamesFileListPrivate
{
  GList *list;
};

/* Remove duplicate names form the list */
static void
games_file_list_remove_duplicates (GamesFileList * filelist)
{
  GList *l;

  if (filelist == NULL)
    return;

  l = filelist->priv->list;

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
  filelist->priv->list = games_file_list_new_internal (glob, paths);
  va_end (paths);

  filelist->priv->list =
    g_list_sort (filelist->priv->list, (GCompareFunc) g_utf8_collate);
  games_file_list_remove_duplicates (filelist);

  return filelist;
}

/* Transform the list of files to be only the basenames. */
void
games_file_list_transform_basename (GamesFileList * filelist)
{
  GList *current = filelist->priv->list;
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

  /* Results in strict-aliasing warning due to glib bug #316221 */
  g_static_mutex_lock (&image_suffix_mutex);

  /* This check needs to be inside the lock to make sure that another
   * thread haasn't half-completed the list. */
  if (image_suffix_list) {
    /* Results in strict-aliasing warning due to glib bug #316221 */
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

  /* Results in strict-aliasing warning due to glib bug #316221 */
  g_static_mutex_unlock (&image_suffix_mutex);
}

static GList *
games_file_list_new_images_single (const gchar * directory)
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
games_file_list_new_images (const gchar * path1, ...)
{
  GamesFileList *filelist;
  GList *list;
  gchar *pathentry;
  va_list paths;

  filelist = g_object_new (GAMES_FILE_LIST_TYPE, NULL);

  filelist->priv->list = games_file_list_new_images_single (path1);
  va_start (paths, path1);
  while ((pathentry = va_arg (paths, gchar *)) != NULL) {
    list = g_list_concat (filelist->priv->list,
			  games_file_list_new_images_single (pathentry));
  }
  va_end (paths);

  filelist->priv->list =
    g_list_sort (filelist->priv->list, (GCompareFunc) g_utf8_collate);
  games_file_list_remove_duplicates (filelist);

  return filelist;
}

/**
 * games_file_list_create_widget:
 * @filelist: The list of files to use.
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
 * Return value: (transfer full): A widget with the list of names.
 **/
GtkWidget *
games_file_list_create_widget (GamesFileList * filelist,
			       const gchar * selection, guint flags)
{
  gint itemno;
  GtkComboBox *widget;
  gchar *visible, *string;
  GList *iter = filelist->priv->list;
  gboolean found = FALSE;

  widget = GTK_COMBO_BOX (gtk_combo_box_text_new ());

  itemno = 0;
  while (iter) {
    gchar *s;

    string = (gchar *) iter->data;
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

    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (widget), visible);
    if (selection && (!strcmp (string, selection))) {
      gtk_combo_box_set_active (widget, itemno);
      found = TRUE;
    }

    g_free (visible);

    itemno++;
    iter = g_list_next (iter);
  }
  if (!found)
    gtk_combo_box_set_active (widget, 0);

  return GTK_WIDGET (widget);
}

/**
 * games_file_list_for_each:
 * @filelist: The file list to iterate over.
 * @function: (scope call): The function to call on each item. It gets called with two 
 * arguments: the file name and the pointer supplied to this function in 
 * the userdata argument.
 * @userdata: (closure): An arbitrary pointer that gets passed as the second argument
 * to each call of function.
 * 
 * Apply a function to each file name in the list.
 **/
void
games_file_list_for_each (GamesFileList * filelist, GFunc function,
                          gpointer userdata)
{
  g_list_foreach (filelist->priv->list, function, userdata);
}

/**
 * games_file_list_find:
 * @filelist: The file list to iterate over.
 * @function: (scope call): The function to call on each item. It gets called with two 
 * arguments: the file name and the pointer supplied to this function in 
 * the userdata argument.
 * @userdata: (closure): An arbitrary pointer that gets passed as the second argument
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

  element = g_list_find_custom (filelist->priv->list, userdata, function);

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
  return (gchar *) g_list_nth_data (filelist->priv->list, n);
}

static void
games_file_list_finalize (GObject * object)
{
  GamesFileList *filelist = GAMES_FILE_LIST (object);

  /* For simplicity we haven't used the dispose method since we can
   * guarantee that everything this references doesn't reference itself. */

  g_list_foreach (filelist->priv->list, (GFunc) g_free, NULL);
  g_list_free (filelist->priv->list);

  G_OBJECT_CLASS (games_file_list_parent_class)->finalize (object);
}

static void
games_file_list_class_init (GamesFileListClass * class)
{
  GObjectClass *oclass = G_OBJECT_CLASS (class);

  oclass->finalize = games_file_list_finalize;

  g_type_class_add_private (oclass, sizeof (GamesFileListPrivate));
}

static void
games_file_list_init (GamesFileList * filelist)
{
  filelist->priv = G_TYPE_INSTANCE_GET_PRIVATE (filelist, GAMES_FILE_LIST_TYPE, GamesFileListPrivate);
}
