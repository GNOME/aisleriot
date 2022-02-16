/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "ar-svg.h"
#include "ar-runtime.h"
#include "ar-string-utils.h"

#include "ar-card-theme.h"
#include "ar-card-theme-private.h"

struct _ArCardThemeSVGClass {
  ArCardThemePreimageClass parent_class;
};

struct _ArCardThemeSVG {
  ArCardThemePreimage parent_instance;
};

#define N_ROWS ((double) 5.0)
#define N_COLS ((double) 13.0)

#define DELTA (0.0f)

/* Class implementation */

G_DEFINE_TYPE (ArCardThemeSVG, ar_card_theme_svg, AR_TYPE_CARD_THEME_PREIMAGE);

static void
ar_card_theme_svg_paint_card (ArCardTheme *card_theme,
                              cairo_surface_t *surface,
                              int card_id)
{
  ArCardThemePreimage *preimage_card_theme = (ArCardThemePreimage *) card_theme;
  ArSvg *svg = preimage_card_theme->cards_svg;
  int suit, rank;
  double card_width, card_height;
  double width, height;
  double offsetx, offsety;
  double zoomx, zoomy;
  char node[32];

  if (G_UNLIKELY (card_id == AR_CARD_SLOT)) {
    ar_svg_render_cairo (preimage_card_theme->slot_preimage,
                                 surface,
                                 preimage_card_theme->card_size.width,
                                 preimage_card_theme->card_size.height);
    return;
  }

  suit = card_id / 13;
  rank = card_id % 13;

  card_width = ((double) ar_svg_get_width (svg)) / N_COLS;
  card_height = ((double) ar_svg_get_height (svg)) / N_ROWS;

  width = preimage_card_theme->card_size.width - 2 * DELTA;
  height = preimage_card_theme->card_size.height - 2 * DELTA;

  offsetx = -((double) rank) * card_width + DELTA;
  offsety = -((double) suit) * card_height + DELTA;

  zoomx = width / card_width;
  zoomy = height / card_height;

  ar_card_get_node_by_suit_and_rank_snprintf (node, sizeof (node), suit, rank);

  ar_svg_render_cairo_sub (svg,
                                   surface,
                                   node,
                                   preimage_card_theme->card_size.width,
                                   preimage_card_theme->card_size.height,
                                   offsetx, offsety,
                                   zoomx, zoomy);
}

static void
ar_card_theme_svg_init (ArCardThemeSVG * cardtheme)
{
}

static ArCardThemeInfo *
ar_card_theme_svg_class_get_theme_info (ArCardThemeClass *klass,
                                           const char *path,
                                           const char *filename)
{
  ArCardThemeInfo *info;

  info = AR_CARD_THEME_CLASS (ar_card_theme_svg_parent_class)->get_theme_info (klass, path, filename);
  if (info) {
    g_assert (info->pref_name == NULL);

    /* SVG is the default. For pref backward compatibility,
     * we don't add a svg: prefix there. We don't strip the .svg
     * extension anymore though.
     */
    info->pref_name = g_strdup (filename);
    return info;
  }

  return NULL;
}

static gboolean
ar_card_theme_svg_class_foreach_theme_dir (ArCardThemeClass *klass,
                                              ArCardThemeForeachFunc callback,
                                              gpointer data)
{
  if (!_ar_card_theme_class_foreach_env (klass, "AR_CARD_THEME_PATH_SVG", callback, data))
    return FALSE;

  if (!callback (klass, ar_runtime_get_directory (AR_RUNTIME_SCALABLE_CARDS_DIRECTORY), data))
    return FALSE;

  /* If we're installed in a non-system prefix, also load the card themes
   * from the system prefix.
   */
  if (!ar_runtime_is_system_prefix ()) {
    if (!callback (klass, "/usr/share/aisleriot/cards", data))
      return FALSE;
  }

  /* Load user-installed themes */
  if (!_ar_card_theme_class_foreach_user_dir (klass, "cards", callback, data))
    return FALSE;

  return TRUE;
}

static void
ar_card_theme_svg_class_init (ArCardThemeSVGClass * klass)
{
  ArCardThemeClass *theme_class = AR_CARD_THEME_CLASS (klass);

  theme_class->get_theme_info = ar_card_theme_svg_class_get_theme_info;
  theme_class->foreach_theme_dir = ar_card_theme_svg_class_foreach_theme_dir;

  theme_class->paint_card = ar_card_theme_svg_paint_card;
}

/* private API */

/**
 * ar_card_theme_svg_new:
 *
 * Returns: a new #ArCardThemeSVG
 */
ArCardTheme*
ar_card_theme_svg_new (void)
{
  return g_object_new (AR_TYPE_CARD_THEME_SVG, NULL);
}
