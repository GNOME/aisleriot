/*
 *  Copyright © 2007, 2008 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3, or (at your option)
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
 */

#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <glib.h>

#include <gtk/gtk.h>

#include "ar-runtime.h"
#include "ar-string-utils.h"

#include "ar-card-theme.h"
#include "ar-card-themes.h"
#include "ar-card-theme-private.h"

int
main (int argc, char *argv[])
{
  GError *err = NULL;
  char *basepath = NULL, *kfname, *kfpath, *theme_filename;
  ArCardThemeInfo *theme_info = NULL;
  ArCardThemes *theme_manager = NULL;
  ArCardTheme *theme = NULL;
  GKeyFile *key_file = NULL;
  int i;
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
    { "theme-directory", 'd', 0, G_OPTION_ARG_FILENAME, &theme_dir,
      "The directory where the scalable card themes are installed", "DIR" },
    { "output-directory", 'o', 0, G_OPTION_ARG_FILENAME, &outpath,
      "Select the output root directoy", "DIR" },
    { "antialias", 'a', 0, G_OPTION_ARG_STRING, &antialias,
      "Enable antialiasing", NULL },
    { "subpixel-order", 's', 0, G_OPTION_ARG_STRING, &subpixel_order,
      "Subpixel order to use when using subpixel antialiasing" },
    { G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY, &args,
      NULL, "SIZE..." },
    { NULL }
  };

  if (!ar_runtime_init ("aisleriot"))
    exit (1);

  if (!gtk_init_with_args
      (&argc, &argv, NULL, (GOptionEntry *) options, NULL, &err)) {
    g_printerr ("Error parsing arguments: %s\n", err->message);
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
      g_printerr
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
      g_printerr
        ("Unknown antialias mode '%s' specified. Valid modes are default, none, grey and subpixel.\n",
         antialias);
      goto loser;
    }
    antialias_set = TRUE;
  }

  if (!outpath) {
    g_printerr ("No output directory specified.\n");
    goto loser;
  }

  if (!theme_name) {
    g_printerr ("No card theme specified.\n");
    goto loser;
  }

  if (!args) {
    g_printerr ("No output size(s) specified.\n");
    goto loser;
  }

  theme_filename = g_strdup_printf ("%s.svg", theme_name);
  theme_info = _ar_card_theme_info_new (AR_TYPE_CARD_THEME_SVG,
                                        theme_dir ? theme_dir : ar_runtime_get_directory (AR_RUNTIME_SCALABLE_CARDS_DIRECTORY),
                                        theme_filename,
                                        ar_filename_to_display_name (theme_name),
                                        g_strdup_printf ("svg:%s", theme_filename) /* FIXMEchpe is this correct? */,
                                        TRUE /* scalable */,
                                        NULL, NULL);
  g_free (theme_filename);
  theme_manager = ar_card_themes_new ();
  theme = ar_card_themes_get_theme (theme_manager, theme_info);
  if (!theme) {
    /* FIXMEchpe print real error */
    g_printerr ("Failed to load theme '%s'\n", theme_name);
    goto loser;
  }

  if (antialias_set) {
    cairo_font_options_t *font_options;

    font_options = cairo_font_options_create ();
    cairo_font_options_set_antialias (font_options, antialias_mode);
    cairo_font_options_set_subpixel_order (font_options, subpixels);
    ar_card_theme_set_font_options (theme, font_options);
    cairo_font_options_destroy (font_options);
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

    ar_card_theme_set_size (theme, size, -1, 1.0);

    g_snprintf (sizestr, sizeof (sizestr), "%u", size);
    sizepath = g_build_filename (basepath, sizestr, NULL);

    if (g_mkdir_with_parents (sizepath, 0755) < 0) {
      g_warning ("Failed to create path %s: %s\n", sizepath,
                 g_strerror (errno));
      goto loser;
    }

    for (j = 0; j < AR_CARDS_TOTAL; ++j) {
      GdkPixbuf *pixbuf;
      char *name, *filename, *path;
      GError *error = NULL;

      pixbuf = ar_card_theme_get_card_pixbuf (theme, j);
      if (!pixbuf)
        goto loser;

      name = ar_card_get_name_by_id (j);

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

    ar_card_theme_get_size (theme, &card_size);
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
  if (theme_info)
    ar_card_theme_info_unref (theme_info);
  if (theme_manager)
    g_object_unref (theme_manager);
  if (key_file)
    g_key_file_free (key_file);

  return retval;
}
