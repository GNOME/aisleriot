/* games-files.c
   Copyright 2003 Callum McKenzie

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

#include <gtk/gtk.h>
#include <stdarg.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "games-files.h"

static GList * games_file_list_new_internal (gchar * glob, va_list path)
{
  GPatternSpec * filespec = g_pattern_spec_new (glob);
  gchar * pathelement;
  GList * list = NULL;
  GDir * dir;
  const gchar * filename;
  gchar * fullname;

  while ((pathelement = va_arg (path, gchar *)) != NULL) {
    dir = g_dir_open (pathelement, 0, NULL);
    if (dir != NULL) {
      while ((filename = g_dir_read_name (dir)) != NULL) {
	if (g_pattern_match_string (filespec, filename)) {
	  fullname = g_build_filename (pathelement, filename, NULL);
	  if (g_file_test (fullname, G_FILE_TEST_IS_REGULAR)) {
	    list = g_list_append (list, fullname);
	  } else g_free (fullname);
	}
      }
      g_dir_close (dir);
    }
  }

  g_pattern_spec_free (filespec);

  return list;
}

/* This function takes a glob and a NULL terminated list of paths 
 * and finds all files in the path matching the glob. Only regular 
 * files are returned. */
/* The arguments are the filespec followed by a null-terminated list 
 * of paths. */
/* The caller must free the list. */
GamesFileList * games_file_list_new (gchar * glob, ...)
{
  GamesFileList * filelist;
  va_list paths;

  filelist = g_object_new (GAMES_FILE_LIST_TYPE, NULL);

  va_start (paths, glob);
  filelist->list = games_file_list_new_internal (glob, paths);
  va_end (paths);

  filelist->list = g_list_sort (filelist->list, (GCompareFunc) g_utf8_collate);

  return filelist;
}

/* Transform the list of files to be only the basenames. */
void games_file_list_transform_basename (GamesFileList * filelist)
{
  GList * current = filelist->list;
  gchar * shortname;
  
  while (current) {
    shortname = g_path_get_basename ((gchar *) current->data);
    g_free (current->data);
    (gchar *) current->data = shortname;
    current = g_list_next (current);
  }
}

GSList * image_suffix_list = NULL;
GStaticMutex image_suffix_mutex = G_STATIC_MUTEX_INIT;

/* We only want to initilise the list of suffixes once, this is
 * the function that does it. It might even be thread safe, not that
 * this has been tested ... */
static void games_image_suffix_list_init (void)
{
  GSList * pixbuf_formats;
  GSList * element;
  GdkPixbufFormat * formats;
  gchar ** suffices;
  gchar ** suffix;

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
      image_suffix_list = g_slist_append (image_suffix_list, g_strdup_printf (".%s", *suffix));
      suffix++;
    }

    g_strfreev (suffices);

    element = g_slist_next (element);
  }

  pixbuf_formats = gdk_pixbuf_get_formats ();

  g_slist_free (pixbuf_formats);

  g_static_mutex_unlock (&image_suffix_mutex);
}

static GList * games_file_list_new_images_single (gchar * directory)
{
  GDir * dir;
  GList * list = NULL;
  const gchar * filename;
  gchar * fullname;
  GSList * suffix;

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
	} else g_free (fullname);
	break;
      } 
      suffix = g_slist_next (suffix);
    }
  }

  g_dir_close (dir);

  return list;
}

/* Get a list of files in the NULL terminated list of directories which are loadable by 
 * GdkPixbuf. Files are identified by their extension. */
GamesFileList * games_file_list_new_images (gchar * path1, ...)
{
  GamesFileList * filelist;
  GList * list;
  gchar * pathentry;
  va_list paths;

  filelist = g_object_new (GAMES_FILE_LIST_TYPE, NULL);

  filelist->list = games_file_list_new_images_single (path1);
  va_start (paths, path1);
  while ((pathentry = va_arg (paths, gchar *)) != NULL) {
    list = g_list_concat (filelist->list, 
			  games_file_list_new_images_single (pathentry));
  }
  va_end (paths);

  filelist->list = g_list_sort (filelist->list, (GCompareFunc) g_utf8_collate);

  return filelist;
}

/* Create a gtk_combo_box with the given list of strings as the
   entries. If selection is given and it matches a list item then that
   item is selected as the default. If selection == NULL then nothing
   is selected. flags is a list of transformations to apply to the 
   names before they are displayed (the actual file list is unaffected). */
GtkWidget * games_file_list_create_widget (GamesFileList * gamesfilelist, gchar * selection, guint flags)
{
  gint itemno;
  GtkComboBox *widget;
  gchar *visible, *string;
  GList * filelist = gamesfilelist->list;
  gboolean found = FALSE;

  widget = GTK_COMBO_BOX (gtk_combo_box_new_text ());

  itemno = 0;
  while (filelist) {
    gchar * s;

    string = (gchar *) filelist->data;
    visible = g_strdup (string);

    /* These are a bit hackish, but we don't yet have a good regexp
     * library in glib. There are probably some ways these could
     * seriously mangle unicode strings. */
    if (flags & GAMES_FILE_LIST_REMOVE_EXTENSION) {
      s = g_strrstr (visible, ".");
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
    if (selection && (! g_utf8_collate (string, selection))) {
      gtk_combo_box_set_active (widget, itemno);      
      found = TRUE;
    }

    itemno++;
    filelist = g_list_next (filelist);
  }
  if (!found)
    gtk_combo_box_set_active (widget, 0);

  return GTK_WIDGET (widget);
}

/* The direct equivalent of g_list_foreach iterated across the 
 * list of filenames. */
void games_file_list_for_each (GamesFileList * filelist, GFunc function, 
			       gpointer userdata)
{
  g_list_foreach (filelist->list, function, userdata);
}


/* Return the nth filename in the list. */
gchar * games_file_list_get_nth (GamesFileList * filelist, gint n)
{
  return (gchar *) g_list_nth_data (filelist->list, n);
}

static void games_file_list_finalize (GamesFileList * filelist)
{
  /* For simplicity we haven't used the dispose method since we can
   * guarantee that everything this references doesn't reference itself. */

  g_list_foreach (filelist->list, (GFunc) g_free, NULL);
  g_list_free (filelist->list);    
}

static void games_file_list_class_init (GamesFileListClass *class)
{
  GObjectClass *oclass = G_OBJECT_CLASS (class);

  oclass->finalize = (GObjectFinalizeFunc) games_file_list_finalize;
}

static void games_file_list_init (GamesFileList *filelist)
{
  filelist->list = NULL;
}

GType games_file_list_get_type (void)
{
  static GType type = 0;
  static const GTypeInfo info = {
    sizeof (GamesFileListClass),
    NULL,
    NULL,
    (GClassInitFunc) games_file_list_class_init,
    NULL,
    NULL,
    sizeof (GamesFileList),
    0,     
    (GInstanceInitFunc) games_file_list_init
  };

  if (!type)
    type = g_type_register_static (G_TYPE_OBJECT, "GamesFileList", &info, 0);

  return type;
}
