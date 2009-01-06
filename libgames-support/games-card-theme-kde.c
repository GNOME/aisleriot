/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008 Christian Persch

  This programme is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This programme is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this programme.  If not, see <http://www.gnu.org/licenses/>. */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "games-find-file.h"
#include "games-files.h"
#include "games-preimage.h"
#include "games-preimage-private.h"
#include "games-runtime.h"
#include "games-string-utils.h"

#include "games-card-theme.h"
#include "games-card-theme-private.h"

struct _GamesCardThemeKDEClass {
  GamesCardThemePreimageClass parent_class;
};

struct _GamesCardThemeKDE {
  GamesCardThemePreimage parent_instance;
};

#include <librsvg/librsvg-features.h>
#if defined(HAVE_RSVG) && defined(LIBRSVG_CHECK_VERSION) && LIBRSVG_CHECK_VERSION(2, 22, 4)
#define HAVE_RSVG_BBOX
#endif

#define N_ROWS ((double) 5.0)
#define N_COLS ((double) 13.0)

#define DELTA (0.0f)

#define KDE_BACKDECK_FILENAME     "index.desktop"
#define KDE_BACKDECK_GROUP        "KDE Backdeck"
#define KDE_BACKDECK_BACK_KEY     "Back"
#define KDE_BACKDECK_BACKSIZE_KEY "BackSize"
#define KDE_BACKDECK_NAME_KEY     "Name" /* localised */
#define KDE_BACKDECK_PYSOL_KEY    "PySol"
#define KDE_BACKDECK_SVG_KEY      "SVG"

/* Class implementation */

G_DEFINE_TYPE (GamesCardThemeKDE, games_card_theme_kde, GAMES_TYPE_CARD_THEME_PREIMAGE);

static gboolean
games_card_theme_kde_load (GamesCardTheme *card_theme,
                           GError **error)
{
  GamesCardThemePreimage *preimage_card_theme = (GamesCardThemePreimage *) card_theme;
  gboolean retval = FALSE;

#ifndef HAVE_RSVG_BBOX
  return FALSE;
#endif

  if (!GAMES_CARD_THEME_CLASS (games_card_theme_kde_parent_class)->load (card_theme, error))
    goto out;

  if (!games_preimage_is_scalable (preimage_card_theme->cards_preimage))
    goto out;

  retval = TRUE;

out:

  return retval;
}

#if 0
static double
games_card_theme_kde_get_card_aspect (GamesCardTheme* card_theme)
{
  GamesCardThemeKDE *theme = (GamesCardThemeKDE *) card_theme;

  /* FIXMEchpe: this doesn't work exactly right for the KDE theme */
  double aspect;
aspect =
      (((double) games_preimage_get_width (theme->cards_preimage))
       * N_ROWS) /
      (((double) games_preimage_get_height (theme->cards_preimage))
       * N_COLS);

  return aspect;
}
#endif

static GdkPixbuf *
games_card_theme_kde_get_card_pixbuf (GamesCardTheme *card_theme,
                                      int card_id)
{
#ifdef HAVE_RSVG_BBOX
  GamesCardThemePreimage *preimage_card_theme = (GamesCardThemePreimage *) card_theme;
  GamesPreimage *preimage = preimage_card_theme->cards_preimage;
  GdkPixbuf *subpixbuf;
  int suit, rank;
  double card_width, card_height;
  double width, height;
  double zoomx, zoomy;
  char node[64];
  RsvgDimensionData dimension;
  RsvgPositionData position;

  suit = card_id / 13;
  rank = card_id % 13;

  if (G_UNLIKELY (card_id == GAMES_CARD_SLOT)) {
    subpixbuf = games_preimage_render (preimage_card_theme->slot_preimage,
                                       preimage_card_theme->card_size.width,
                                       preimage_card_theme->card_size.height);

    return subpixbuf;
  }

  games_card_get_node_by_suit_and_rank_snprintf (node, sizeof (node), suit, rank);

#ifdef DEBUG_chpe
  clock_t t1, t2, t3, t4;

  t1 = clock ();
#endif

  if (!rsvg_handle_get_dimensions_sub (preimage->rsvg_handle, &dimension, node)) {
    g_print ("Failed to get dim for '%s'\n", node);
    return NULL;
  }

#ifdef DEBUG_chpe
  t2 = clock ();
  g_print ("took %.3fs to get-dimension card %s\n",
           (t2 - t1) * 1.0 / CLOCKS_PER_SEC, node);
#endif

  if (!rsvg_handle_get_position_sub (preimage->rsvg_handle, &position, node)) {
    g_print ("Failed to get pos for '%s'\n", node);
    return NULL;
  }

#ifdef DEBUG_chpe
  t3 = clock ();
  g_print ("took %.3fs to get-position card %s (cumulative: %.3fs)\n",
           (t3 - t2) * 1.0 / CLOCKS_PER_SEC, node,
           (t3 - t1)* 1.0 / CLOCKS_PER_SEC);

  g_print ("card %s position %d:%d dimension %d:%d\n", node, position.x, position.y, dimension.width, dimension.height);
#endif

  card_width = ((double) games_preimage_get_width (preimage)) / N_COLS;
  card_height = ((double) games_preimage_get_height (preimage)) / N_ROWS;

  width = preimage_card_theme->card_size.width;
  height = preimage_card_theme->card_size.height;

  zoomx = width / card_width;
  zoomy = height / card_height;

//   zoomx = width / dimension.width;
//   zoomy = height / dimension.height;

  subpixbuf = games_preimage_render_sub (preimage,
                                         node,
                                         preimage_card_theme->card_size.width,
                                         preimage_card_theme->card_size.height,
                                         -position.x, -position.y,
                                         zoomx, zoomy);
#ifdef DEBUG_chpe
  t4 = clock ();
  g_print ("took %.3fs to render card %s (cumulative: %.3fs)\n",
           (t4 - t3) * 1.0 / CLOCKS_PER_SEC, node,
           (t4 - t1)* 1.0 / CLOCKS_PER_SEC);

  g_print ("Returning %p\n", subpixbuf);
#endif

  return subpixbuf;
#else
  return NULL;
#endif
}

static void
games_card_theme_kde_init (GamesCardThemeKDE * cardtheme)
{
}

static GamesCardThemeInfo *
games_card_theme_kde_class_get_theme_info (GamesCardThemeClass *klass,
                                           const char *path,
                                           const char *filename)
{
  GamesCardThemeInfo *info = NULL;
  char *base_path = NULL, *key_file_path = NULL;
  GKeyFile *key_file = NULL;
  char *svg_filename = NULL, *display_name = NULL;

  base_path = g_build_filename (path, filename, NULL);
  if (!g_file_test (path, G_FILE_TEST_IS_DIR))
    goto out;

  key_file_path = g_build_filename (base_path, KDE_BACKDECK_FILENAME, NULL);
  key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (key_file, key_file_path, 0, NULL))
    goto out;
      
  if (!g_key_file_has_group (key_file, KDE_BACKDECK_GROUP))
    goto out;

  display_name = g_key_file_get_locale_string (key_file, KDE_BACKDECK_GROUP, KDE_BACKDECK_NAME_KEY, NULL, NULL);
  svg_filename = g_key_file_get_string (key_file, KDE_BACKDECK_GROUP, KDE_BACKDECK_SVG_KEY, NULL);
  if (!display_name || !display_name[0] || !svg_filename || !svg_filename[0])
    goto out;

  display_name = games_filename_to_display_name (svg_filename);

  info = _games_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                     base_path,
                                     svg_filename,
                                     display_name,
                                     NULL, NULL);

out:
  g_free (base_path);
  g_free (key_file_path);
  g_free (svg_filename);
  g_free (display_name);
  if (key_file) {
    g_key_file_free (key_file);
  }

  return info;
}

static void
games_card_theme_kde_class_get_theme_infos (GamesCardThemeClass *klass,
                                            GList **list)
{
  _games_card_theme_class_append_theme_info_foreach_env
    (klass, "GAMES_CARD_THEME_PATH_KDE", list);

  /* FIXMEchpe: is this universal, or ubuntu specific? */
  _games_card_theme_class_append_theme_info_foreach
    (klass, "/usr/share/kde4/apps/carddecks", list);
}

static void
games_card_theme_kde_class_init (GamesCardThemeKDEClass * klass)
{
  GamesCardThemeClass *theme_class = GAMES_CARD_THEME_CLASS (klass);

  theme_class->get_theme_info = games_card_theme_kde_class_get_theme_info;
  theme_class->get_theme_infos = games_card_theme_kde_class_get_theme_infos;

  theme_class->load = games_card_theme_kde_load;
//   theme_class->get_card_aspect = games_card_theme_kde_get_card_aspect;
  theme_class->get_card_pixbuf = games_card_theme_kde_get_card_pixbuf;
}

/* private API */

/**
 * games_card_theme_kde_new:
 *
 * Returns: a new #GamesCardThemeKDE
 */
GamesCardTheme*
games_card_theme_kde_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_THEME_KDE, NULL);
}
