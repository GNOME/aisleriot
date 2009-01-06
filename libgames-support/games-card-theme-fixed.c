/*
   Copyright © 2004 Callum McKenzie
   Copyright © 2007, 2008 Christian Persch

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

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "games-find-file.h"
#include "games-files.h"
#include "games-preimage.h"
#include "games-runtime.h"

#include "games-card-theme.h"
#include "games-card-theme-private.h"

struct _GamesCardThemeFixedClass {
  GamesCardThemeClass parent_class;
};

struct _GamesCardThemeFixed {
  GamesCardTheme parent_instance;

  char *theme_dir;
  char *theme_name;

  /* Switched on GamesCardThemeFixed.use_scalable */
  char *themesizepath;
  CardSize *card_sizes;
  guint n_card_sizes;

  CardSize slot_size;
  CardSize card_size;

  guint use_scalable : 1;
  guint theme_loaded : 1;
  guint size_available : 1;

  guint subrender : 1;
  guint prescaled : 1;
};

/* #defining this prints out the time it takes to render the theme */
/* #define INSTRUMENT_LOADING */

#ifdef INSTRUMENT_LOADING
static long totaltime = 0;
#endif

/* Class implementation */

G_DEFINE_TYPE (GamesCardThemeFixed, games_card_theme_fixed, GAMES_TYPE_CARD_THEME);

static void
games_card_theme_fixed_clear_theme_data (GamesCardThemeFixed *theme)
{
#ifdef INSTRUMENT_LOADING
  /* Reset the time */
  totaltime = 0;
#endif

  g_free (theme->theme_name);
  theme->theme_name = NULL;

  g_free (theme->theme_dir);
  theme->theme_dir = NULL;

  g_free (theme->card_sizes);
  theme->card_sizes = NULL;
  theme->n_card_sizes = 0;

  g_free (theme->themesizepath);
  theme->themesizepath = NULL;

  theme->size_available = FALSE;

  theme->theme_loaded = FALSE;

  theme->card_size.width = theme->card_size.height = theme->slot_size.width =
    theme->slot_size.width = -1;
}

static gboolean
games_card_theme_fixed_load_theme (GamesCardTheme *card_theme,
                                   const char *theme_dir,
                                   const char *theme_name,
                                   GError **error__)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;
  GKeyFile *key_file;
  char *filename, *path;
  GError *error = NULL;
  int *sizes = NULL;
  gsize n_sizes, i;
  gboolean retval = FALSE;

  if (theme->theme_name != NULL &&
      strcmp (theme_name, theme->theme_name) == 0)
    return TRUE;

  games_card_theme_fixed_clear_theme_data (theme);

  if (!theme_dir)
    return FALSE;

  filename = g_strdup_printf ("%s.card-theme", theme_name);
  path = g_build_filename (theme_dir, filename, NULL);
  g_free (filename);

  key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (key_file, path, 0, &error)) {
    g_warning ("Failed to load prerendered card theme from %s: %s\n", path,
               error->message);
    g_error_free (error);
    goto loser;
  }

  sizes =
    g_key_file_get_integer_list (key_file, "Card Theme", "Sizes", &n_sizes,
                                 &error);
  if (error) {
    g_warning ("Failed to get card sizes: %s\n", error->message);
    g_error_free (error);
    goto loser;
  }

  if (n_sizes == 0) {
    g_warning ("Card theme contains no sizes\n");
    goto loser;
  }

  theme->card_sizes = g_new (CardSize, n_sizes);
  theme->n_card_sizes = n_sizes;

  for (i = 0; i < n_sizes; ++i) {
    char group[32];
    GError *err = NULL;
    int width, height;

    g_snprintf (group, sizeof (group), "%d", sizes[i]);

    width = g_key_file_get_integer (key_file, group, "Width", &err);
    if (err) {
      g_warning ("Error loading width for size %d: %s\n", sizes[i],
                 err->message);
      g_error_free (err);
      goto loser;
    }
    height = g_key_file_get_integer (key_file, group, "Height", &err);
    if (err) {
      g_warning ("Error loading height for size %d: %s\n", sizes[i],
                 err->message);
      g_error_free (err);
      goto loser;
    }

    theme->card_sizes[i].width = width;
    theme->card_sizes[i].height = height;
  }

  retval = TRUE;
    
  theme->theme_name = g_strdup (theme_name);
  theme->theme_dir = g_strdup (theme_dir);

  theme->theme_loaded = TRUE;

loser:

  g_free (sizes);

  g_key_file_free (key_file);
  g_free (path);

  if (!retval) {
    games_card_theme_fixed_clear_theme_data (theme);
  }

  _games_card_theme_emit_changed (card_theme);

  return retval;
}

static GdkPixbuf *
games_card_theme_fixed_load_card (GamesCardThemeFixed *theme,
                                  int card_id)
{
  GdkPixbuf *pixbuf;
  GError *error = NULL;
  char name[64], filename[64];
  char *path;

  if (!theme->theme_loaded || !theme->size_available)
    return NULL;

  g_return_val_if_fail (!theme->use_scalable, NULL);

  games_card_get_name_by_id_snprintf (name, sizeof (name), card_id);
  g_snprintf (filename, sizeof (filename), "%s.png", name);
  path = g_build_filename (theme->themesizepath, filename, NULL);

  pixbuf = gdk_pixbuf_new_from_file (path, &error);
  if (!pixbuf) {
    g_warning ("Failed to load card image %s: %s\n", filename,
               error->message);
    g_error_free (error);
  }

  g_free (path);

  return pixbuf;
}

static void
games_card_theme_fixed_finalize (GObject * object)
{
  GamesCardThemeFixed *theme = GAMES_CARD_THEME_FIXED (object);

  games_card_theme_fixed_clear_theme_data (theme);

  G_OBJECT_CLASS (games_card_theme_fixed_parent_class)->finalize (object);
}

static void
games_card_theme_fixed_init (GamesCardThemeFixed *theme)
{
  theme->card_size.width = theme->card_size.height = -1;
  theme->theme_name = NULL;
}

static const gchar *
games_card_theme_fixed_get_theme_name (GamesCardTheme *card_theme)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;

  return theme->theme_name;
}

static gboolean
games_card_theme_fixed_set_card_size (GamesCardTheme *card_theme,
                                      int width,
                                      int height,
                                      double proportion)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;

  if (!theme->theme_loaded) {
    g_warning ("Theme not loaded yet; cannot set size!");
    return FALSE;
  }

  if ((width == theme->slot_size.width) &&
      (height == theme->slot_size.height))
    return FALSE;

  theme->slot_size.width = width;
  theme->slot_size.height = height;

  {
    guint i;
    int twidth, theight;
    CardSize size = { -1, -1 }, fit_size = { -1, -1};

    twidth = FLOAT_TO_INT_CEIL (((double) width) * proportion);
    theight = FLOAT_TO_INT_CEIL (((double) height) * proportion);

    /* Find the closest prerendered size */
    for (i = 0; i < theme->n_card_sizes; ++i) {
      CardSize info = theme->card_sizes[i];

      if (info.width > width || info.height > height)
        continue;

      if (info.width > fit_size.width && info.height > fit_size.height)
        fit_size = info;

      /* FIXMEchpe */
      if (info.width <= twidth && info.height <= theight &&
          info.width > size.width && info.height > size.height)
        size = info;
    }

    if (size.width < 0 || size.height < 0)
      size = fit_size;

    if (size.width > 0 && size.height > 0) {
      char sizestr[16];

      if (size.width == theme->card_size.width &&
          size.height == theme->card_size.height)
        return FALSE;

      g_free (theme->themesizepath);

      g_snprintf (sizestr, sizeof (sizestr), "%d", size.width);
      theme->themesizepath =
        g_build_filename (theme->theme_dir, theme->theme_name, sizestr, NULL);

      theme->size_available = TRUE;
      theme->card_size = size;
    } else {
      g_warning ("No prerendered size available for %d:%d\n", width, height);
      theme->size_available = FALSE;

      /* FIXMEchpe: at least use the smallest available size here, or
       * programme will surely crash when trying to render NULL pixbufs
       * later on!
       */
      return FALSE;
    }
  }

  _games_card_theme_emit_changed (card_theme);

  return TRUE;
}

static CardSize
games_card_theme_fixed_get_card_size (GamesCardTheme *card_theme)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;

  return theme->card_size;
}

static double
games_card_theme_fixed_get_card_aspect (GamesCardTheme *card_theme)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;

  return ((double) theme->card_size.width) / ((double) theme->card_size.height);
}

static GdkPixbuf *
games_card_theme_fixed_get_card_pixbuf (GamesCardTheme *card_theme,
                                        int card_id)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;
  GdkPixbuf *pixbuf;

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  pixbuf = games_card_theme_fixed_load_card (theme, card_id);

#ifdef INSTRUMENT_LOADING
  t2 = clock ();
  totaltime += (t2 - t1);
  g_print ("took %.3fs to render card %d (cumulative: %.3fs)\n",
           (t2 - t1) * 1.0 / CLOCKS_PER_SEC, card_id,
           totaltime * 1.0 / CLOCKS_PER_SEC);
#endif

  return pixbuf;
}

static const char *
games_card_theme_fixed_get_default_theme_path (GamesCardThemeClass *klass)
{
  const char *env;

  if ((env = g_getenv ("GAMES_CARD_THEME_PATH_FIXED")))
    return env;
  if ((env = g_getenv ("GAMES_CARD_THEME_PATH")))
    return env;

  return games_runtime_get_directory (GAMES_RUNTIME_PRERENDERED_CARDS_DIRECTORY);
}

static const char *
games_card_theme_fixed_get_theme_glob (GamesCardThemeClass *klass)
{
  return "*.card-theme";
}

static void
games_card_theme_fixed_class_init (GamesCardThemeFixedClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GamesCardThemeClass *theme_class = GAMES_CARD_THEME_CLASS (klass);

  gobject_class->finalize = games_card_theme_fixed_finalize;

  theme_class->get_default_theme_path = games_card_theme_fixed_get_default_theme_path;
  theme_class->get_theme_glob = games_card_theme_fixed_get_theme_glob;
  theme_class->load_theme = games_card_theme_fixed_load_theme;
  theme_class->get_theme_name = games_card_theme_fixed_get_theme_name;
  theme_class->set_card_size = games_card_theme_fixed_set_card_size;
  theme_class->get_card_size = games_card_theme_fixed_get_card_size;
  theme_class->get_card_aspect = games_card_theme_fixed_get_card_aspect;
  theme_class->get_card_pixbuf = games_card_theme_fixed_get_card_pixbuf;
}

/* public API */

/**
 * games_card_theme_fixed_new:
 *
 * Returns: a new #GamesCardThemeFixed
 */
GamesCardTheme *
games_card_theme_fixed_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_THEME_FIXED, NULL);
}
