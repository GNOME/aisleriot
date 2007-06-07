/*
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *  $Id$
 */

#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <glib.h>
#include <glib/gthread.h>

#include <gtk/gtk.h>

#include "games-card-theme.h"

int
main (int argc, char *argv[])
{
  GError *err = NULL;
  char *basepath = NULL, *kfname, *kfpath;
  GamesCardTheme *theme = NULL;
  GKeyFile *key_file = NULL;
  guint i;
  int retval = EXIT_FAILURE;
  int *sizes = NULL, n_sizes = 0;
  char *data = NULL;
  gsize len;
  cairo_subpixel_order_t subpixels = CAIRO_SUBPIXEL_ORDER_DEFAULT;
  cairo_antialias_t antialias_mode = CAIRO_ANTIALIAS_DEFAULT;
  char *outpath = NULL, *theme_name = NULL, *theme_dir =
    NULL, *subpixel_order = NULL, *antialias = NULL;
  char **args = NULL;
  gboolean antialias_set = FALSE;
  const GOptionEntry options[] = {
    { "theme", 't', 0, G_OPTION_ARG_STRING, &theme_name,
      "Select the theme to render", "NAME" },
    { "theme-directory", 'd', 0, G_OPTION_ARG_STRING, &theme_dir,
      "The directory where the scalable card themes are installed", "DIR" },
    { "output-directory", 'o', 0, G_OPTION_ARG_STRING, &outpath,
      "Select the output root directoy", "DIR" },
    { "antialias", 'a', 0, G_OPTION_ARG_STRING, &antialias,
      "Enable antialiasing", NULL },
    { "subpixel-order", 's', 0, G_OPTION_ARG_STRING, &subpixel_order,
      "Subpixel order to use when using subpixel antialiasing" },
    { G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY, &args,
      NULL, "SIZE..." },
    { NULL }
  };

#if defined(HAVE_GNOME) || defined(HAVE_RSVG_GNOMEVFS)
  /* If we're going to use gnome-vfs, we need to init threads before
   * calling any glib functions.
   */
  g_thread_init (NULL);
#endif

  if (!gtk_init_with_args
      (&argc, &argv, NULL, (GOptionEntry *) options, NULL, &err)) {
    g_print ("Error parsing arguments: %s\n", err->message);
    g_error_free (err);
    goto loser;
  }

  if (subpixel_order != NULL) {
    if (g_ascii_strcasecmp (subpixel_order, "none") == 0) {
      subpixels = CAIRO_SUBPIXEL_ORDER_DEFAULT;
    } else if (g_ascii_strcasecmp (subpixel_order, "rgb") == 0) {
      subpixels = CAIRO_SUBPIXEL_ORDER_RGB;
    } else if (g_ascii_strcasecmp (subpixel_order, "bgr") == 0) {
      subpixels = CAIRO_SUBPIXEL_ORDER_BGR;
    } else if (g_ascii_strcasecmp (subpixel_order, "vrgb") == 0) {
      subpixels = CAIRO_SUBPIXEL_ORDER_VRGB;
    } else if (g_ascii_strcasecmp (subpixel_order, "vbgr") == 0) {
      subpixels = CAIRO_SUBPIXEL_ORDER_VBGR;
    } else {
      g_print
        ("Unknown subpixel order '%s' specified. Valid orders are none, rgb, bgr, vrgb, vbgr.\n",
         subpixel_order);
      goto loser;
    }
  }

  if (antialias != NULL) {
    if (g_ascii_strcasecmp (antialias, "default") == 0) {
      antialias_mode = CAIRO_ANTIALIAS_DEFAULT;
    } else if (g_ascii_strcasecmp (antialias, "none") == 0) {
      antialias_mode = CAIRO_ANTIALIAS_NONE;
    } else if (g_ascii_strcasecmp (antialias, "grey") == 0) {
      antialias_mode = CAIRO_ANTIALIAS_GRAY;
    } else if (g_ascii_strcasecmp (antialias, "subpixel") == 0) {
      antialias_mode = CAIRO_ANTIALIAS_SUBPIXEL;
    } else {
      g_print
        ("Unknown antialias mode '%s' specified. Valid modes are default, none, grey and subpixel.\n",
         antialias);
      goto loser;
    }
    antialias_set = TRUE;
  }

  if (!outpath) {
    g_print ("No output directory specified.\n");
    goto loser;
  }

  if (!theme_name) {
    g_print ("No card theme specified.\n");
    goto loser;
  }

  if (!args) {
    g_print ("No output size(s) specified.\n");
    goto loser;
  }

  theme = games_card_theme_new (NULL, TRUE);
  if (antialias_set) {
    games_card_theme_set_antialias (theme, antialias_mode, subpixels);
  }

  if (!games_card_theme_set_theme (theme, theme_name)) {
    g_warning ("Failed to load theme '%s'\n", theme_name);
    goto loser;
  }

  if (g_mkdir_with_parents (outpath, 0755) < 0) {
    g_warning ("Failed to create path %s: %s\n", outpath, g_strerror (errno));
    goto loser;
  }

  basepath = g_build_filename (outpath, theme_name, NULL);

  key_file = g_key_file_new ();

  n_sizes = g_strv_length (args);
  sizes = g_new (int, n_sizes);

  for (i = 0; i < n_sizes; ++i) {
    guint size, j;
    char sizestr[32];
    char *sizepath;
    CardSize card_size;

    errno = 0;

    size = g_ascii_strtoull (args[i], NULL, 10);
    if (size == 0 || errno != 0)
      goto loser;

    games_card_theme_set_size (theme, size, -1, 1.0);

    g_snprintf (sizestr, sizeof (sizestr), "%d", size);
    sizepath = g_build_filename (basepath, sizestr, NULL);

    if (g_mkdir_with_parents (sizepath, 0755) < 0) {
      g_warning ("Failed to create path %s: %s\n", sizepath,
                 g_strerror (errno));
      goto loser;
    }

    for (j = 0; j < GAMES_CARDS_TOTAL; ++j) {
      GdkPixbuf *pixbuf;
      char *name, *filename, *path;
      GError *error = NULL;

      pixbuf = games_card_theme_get_card_pixbuf (theme, j);
      if (!pixbuf)
        goto loser;

      name = games_card_theme_get_card_name (theme, j);

      filename = g_strdup_printf ("%s.png", name);
      path = g_build_filename (sizepath, filename, NULL);

      if (!gdk_pixbuf_save (pixbuf, path, "png", &error, NULL)) {
        g_warning ("Failed to save %s: %s\n", name, error->message);
        g_error_free (error);
        g_object_unref (pixbuf);
        goto loser;
      }

      g_free (name);
      g_free (filename);
      g_free (path);
      g_object_unref (pixbuf);
    }

    g_free (sizepath);

    sizes[i] = size;

    card_size = games_card_theme_get_size (theme);
    g_key_file_set_integer (key_file, sizestr, "Width", card_size.width);
    g_key_file_set_integer (key_file, sizestr, "Height", card_size.height);
  }

  g_key_file_set_integer_list (key_file, "Card Theme", "Sizes", sizes,
                               n_sizes);

  data = g_key_file_to_data (key_file, &len, NULL);
  if (!data)
    goto loser;

  kfname = g_strdup_printf ("%s.card-theme", theme_name);
  kfpath = g_build_filename (outpath, kfname, NULL);
  g_free (kfname);

  if (!g_file_set_contents (kfpath, data, len, &err)) {
    g_warning ("Failed to write key file %s: %s\n", kfpath, err->message);
    g_error_free (err);
    g_free (kfpath);
    goto loser;
  }

  g_free (kfpath);

  retval = EXIT_SUCCESS;

loser:

  g_free (theme_name);
  g_free (theme_dir);
  g_free (outpath);
  g_free (antialias);
  g_free (subpixel_order);
  g_free (basepath);
  g_free (sizes);
  g_free (data);
  g_strfreev (args);
  if (theme)
    g_object_unref (theme);
  if (key_file)
    g_key_file_free (key_file);

  return retval;
}
