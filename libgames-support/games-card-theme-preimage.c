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

#define N_ROWS ((double) 5.0)
#define N_COLS ((double) 13.0)

#define DELTA (0.0f)

/* #defining this prints out the time it takes to render the theme */
/* #define INSTRUMENT_LOADING */

#ifdef INSTRUMENT_LOADING
static long totaltime = 0;
#endif

/* Class implementation */

G_DEFINE_ABSTRACT_TYPE (GamesCardThemePreimage, games_card_theme_preimage, GAMES_TYPE_CARD_THEME);

void
_games_card_theme_preimage_clear_sized_theme_data (GamesCardThemePreimage *theme)
{
  void (* clear_sized_theme_data) (GamesCardThemePreimage *) =
    GAMES_CARD_THEME_PREIMAGE_GET_CLASS (theme)->clear_sized_theme_data;

  if (clear_sized_theme_data)
    clear_sized_theme_data (theme);

#ifdef INSTRUMENT_LOADING
  /* Reset the time */
  totaltime = 0;
#endif
}

static gboolean
games_card_theme_preimage_load (GamesCardTheme *card_theme,
                                GError **error)
{
  GamesCardThemePreimage *theme = (GamesCardThemePreimage *) card_theme;
  GamesCardThemeInfo *theme_info = card_theme->theme_info;
  GamesPreimage *preimage;
  const char *slot_dir;
  char *path;

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  // XXX FIXMEchpe remove this crap
  /* First try and load the given file. */
  //filename = g_strdup_printf ("%s.svg", theme_name);
  path = g_build_filename (theme_info->path, theme_info->filename, NULL);
  preimage = games_preimage_new_from_file (path, NULL);
  g_free (path);

  /* Failing that, try and find a similar file (e.g. a suffix change). */
  if (!preimage) {
    // FIXMEchpe remove this crap
    path = games_find_similar_file (theme_info->filename, theme_info->path);
    if (path) {
      preimage = games_preimage_new_from_file (path, NULL);
      g_free (path);
    }
  }

  if (!preimage)
    goto out;

  if (theme->font_options) {
    games_preimage_set_font_options (preimage, theme->font_options);
  }

  theme->cards_preimage = preimage;

  /* And the slot image */
  /* FIXMEchpe: use uninstalled data dir for rendering the card theme! */
  slot_dir = games_runtime_get_directory (GAMES_RUNTIME_PIXMAP_DIRECTORY);
  path = g_build_filename (slot_dir, "slot.svg", NULL);
  theme->slot_preimage = games_preimage_new_from_file (path, NULL);
  g_free (path);
  // FIXMEchpe fix this
  g_return_val_if_fail (theme->slot_preimage != NULL, FALSE);

  if (theme->font_options) {
    games_preimage_set_font_options (theme->slot_preimage,
                                     theme->font_options);
  }

out:

#ifdef INSTRUMENT_LOADING
  t2 = clock ();
  totaltime += (t2 - t1);
  g_print ("took %.3fs to create preimage (cumulative %.3fs)\n",
           (t2 - t1) * 1.0 / CLOCKS_PER_SEC,
           totaltime * 1.0 / CLOCKS_PER_SEC);
#endif

  return theme->cards_preimage != NULL;
}

static void
games_card_theme_preimage_init (GamesCardThemePreimage *theme)
{
  theme->cards_preimage = NULL;
  theme->slot_preimage = NULL;

  theme->subsize.width = -1;
  theme->subsize.height = -1;

  theme->theme_loaded = FALSE;

  theme->card_size.width = theme->card_size.height = theme->slot_size.width =
    theme->slot_size.width = -1;
}

static void
games_card_theme_preimage_finalize (GObject * object)
{
  GamesCardThemePreimage *theme = GAMES_CARD_THEME_PREIMAGE (object);

  _games_card_theme_preimage_clear_sized_theme_data (theme);

  if (theme->cards_preimage != NULL) {
    g_object_unref (theme->cards_preimage);
  }
  if (theme->slot_preimage != NULL) {
    g_object_unref (theme->slot_preimage);
  }

  if (theme->font_options) {
    cairo_font_options_destroy (theme->font_options);
  }

  G_OBJECT_CLASS (games_card_theme_preimage_parent_class)->finalize (object);
}

static void
games_card_theme_preimage_set_font_options (GamesCardTheme *card_theme,
                                            const cairo_font_options_t *font_options)
{
  GamesCardThemePreimage *theme = (GamesCardThemePreimage *) card_theme;

  if (font_options &&
      theme->font_options &&
      cairo_font_options_equal (font_options, theme->font_options))
    return;

  if (theme->font_options) {
    cairo_font_options_destroy (theme->font_options);
  }

  if (font_options) {
    theme->font_options = cairo_font_options_copy (font_options);
  } else {
    theme->font_options = NULL;
  }

  _games_card_theme_preimage_clear_sized_theme_data (theme);
  _games_card_theme_emit_changed (card_theme);
}

static gboolean
games_card_theme_preimage_set_card_size (GamesCardTheme *card_theme,
                                         int width,
                                         int height,
                                         double proportion)
{
  GamesCardThemePreimage *theme = (GamesCardThemePreimage *) card_theme;
  double aspect_ratio, twidth, theight;

  if ((width == theme->slot_size.width) &&
      (height == theme->slot_size.height))
    return FALSE;

  theme->slot_size.width = width;
  theme->slot_size.height = height;

  /* Now calculate the card size: find the maximum size that fits
   * into the given area, preserving the card's aspect ratio.
   */
  aspect_ratio = games_card_theme_get_aspect (card_theme);

  twidth = proportion * width;
  theight = proportion * height;
  if (twidth / theight < aspect_ratio) {
    theight = twidth / aspect_ratio;
  } else {
    twidth = theight * aspect_ratio;
  }

  if (theme->card_size.width == (int) twidth &&
      theme->card_size.height == (int) theight)
    return FALSE;

  theme->card_size.width = twidth;
  theme->card_size.height = theight;

  _games_card_theme_preimage_clear_sized_theme_data (theme);
  _games_card_theme_emit_changed (card_theme);

  return TRUE;
}

static CardSize
games_card_theme_preimage_get_card_size (GamesCardTheme *card_theme)
{
  GamesCardThemePreimage *theme = (GamesCardThemePreimage *) card_theme;

  return theme->card_size;
}

static double
games_card_theme_preimage_get_card_aspect (GamesCardTheme* card_theme)
{
  GamesCardThemePreimage *theme = (GamesCardThemePreimage *) card_theme;
  double aspect;
aspect =
      (((double) games_preimage_get_width (theme->cards_preimage))
       * N_ROWS) /
      (((double) games_preimage_get_height (theme->cards_preimage))
       * N_COLS);

  return aspect;
}

static GamesCardThemeInfo *
games_card_theme_preimage_class_get_theme_info (GamesCardThemeClass *klass,
                                                const char *path,
                                                const char *filename)
{
  if (!g_str_has_suffix (filename, ".svg")) // FIXMEchpe .svgz
    return NULL;

  return _games_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                     path,
                                     filename, /* FIXME */
                                     filename, /* FIXME */
                                     NULL, NULL);
}

// FIXMEchpe move this to derived classes
static void
games_card_theme_preimage_class_get_theme_infos (GamesCardThemeClass *klass,
                                                 GList **list)
{
  _games_card_theme_class_append_theme_info_foreach_env
    (klass, "GAMES_CARD_THEME_PATH_PREIMAGE", list);

  _games_card_theme_class_append_theme_info_foreach
    (klass, games_runtime_get_directory (GAMES_RUNTIME_SCALABLE_CARDS_DIRECTORY), list);
}

static void
games_card_theme_preimage_class_init (GamesCardThemePreimageClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GamesCardThemeClass *theme_class = GAMES_CARD_THEME_CLASS (klass);

  gobject_class->finalize = games_card_theme_preimage_finalize;

  theme_class->get_theme_info = games_card_theme_preimage_class_get_theme_info;
  theme_class->get_theme_infos = games_card_theme_preimage_class_get_theme_infos;

  theme_class->load = games_card_theme_preimage_load;
  theme_class->set_card_size = games_card_theme_preimage_set_card_size;
  theme_class->get_card_size = games_card_theme_preimage_get_card_size;
  theme_class->get_card_aspect = games_card_theme_preimage_get_card_aspect;
  theme_class->get_card_pixbuf = NULL;
  theme_class->set_font_options = games_card_theme_preimage_set_font_options;
}
