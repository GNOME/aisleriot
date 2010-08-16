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

#include <libgames-support/games-preimage.h>
#include <libgames-support/games-profile.h>
#include <libgames-support/games-runtime.h>
#include <libgames-support/games-string-utils.h>

#include "ar-card-theme.h"
#include "ar-card-theme-private.h"

struct _ArCardThemeSlicedClass {
  ArCardThemePreimageClass parent_class;
};

struct _ArCardThemeSliced {
  ArCardThemePreimage parent_instance;

  GdkPixbuf *source;
  CardSize subsize;

  guint scalable : 1;
};

#define N_ROWS ((double) 5.0)
#define N_COLS ((double) 13.0)

#define DELTA (0.0f)

/* Class implementation */

G_DEFINE_TYPE (ArCardThemeSliced, ar_card_theme_sliced, AR_TYPE_CARD_THEME_PREIMAGE);

static void
ar_card_theme_sliced_clear_sized_theme_data (ArCardThemePreimage *preimage_card_theme)
{
  ArCardThemeSliced *theme = AR_CARD_THEME_SLICED (preimage_card_theme);

  if (theme->scalable &&
      theme->source) {
    g_object_unref (theme->source);
    theme->source = NULL;
  }
}

static gboolean
ar_card_theme_sliced_load (ArCardTheme *card_theme,
                              GError **error)
{
  ArCardThemePreimage *preimage_card_theme = (ArCardThemePreimage *) card_theme;
  ArCardThemeSliced *theme = (ArCardThemeSliced *) card_theme;

  if (!AR_CARD_THEME_CLASS (ar_card_theme_sliced_parent_class)->load (card_theme, error))
    return FALSE;

  /* If we don't have a scalable format, build an unscaled pixbuf that we'll cut up later */
  theme->scalable = games_preimage_is_scalable (preimage_card_theme->cards_preimage);
  if (!theme->scalable) {
    theme->source = games_preimage_render_unscaled_pixbuf (preimage_card_theme->cards_preimage);

    /* This is true because in the non-scalable case GamesPreimage directly holds a GdkPixbuf */
    g_assert (theme->source != NULL);
  
    theme->subsize.width = gdk_pixbuf_get_width (theme->source) / 13;
    theme->subsize.height = gdk_pixbuf_get_height (theme->source) / 5;
  }

  return TRUE;
}

static gboolean
ar_card_theme_sliced_prerender_scalable (ArCardThemeSliced *theme)
{
  ArCardThemePreimage *preimage_card_theme = (ArCardThemePreimage *) theme;

  g_assert (theme->source == NULL);

  _games_profile_start ("prerendering source pixbuf for %s card theme %s", G_OBJECT_TYPE_NAME (theme), ((ArCardTheme*)theme)->theme_info->display_name);

  theme->source = games_preimage_render (preimage_card_theme->cards_preimage,
                                         preimage_card_theme->card_size.width * 13,
                                         preimage_card_theme->card_size.height * 5);

  _games_profile_end ("prerendering source pixbuf for %s card theme %s", G_OBJECT_TYPE_NAME (theme), ((ArCardTheme*)theme)->theme_info->display_name);

  if (!theme->source)
    return FALSE;

  theme->subsize.width = gdk_pixbuf_get_width (theme->source) / 13;
  theme->subsize.height = gdk_pixbuf_get_height (theme->source) / 5;

  return TRUE;
}

static GdkPixbuf *
ar_card_theme_sliced_get_card_pixbuf (ArCardTheme *card_theme,
                                         int card_id)
{
  ArCardThemePreimage *preimage_card_theme = (ArCardThemePreimage *) card_theme;
  ArCardThemeSliced *theme = (ArCardThemeSliced *) card_theme;
  GdkPixbuf *subpixbuf, *card_pixbuf;
  int suit, rank;

  if (G_UNLIKELY (card_id == AR_CARD_SLOT)) {
    subpixbuf = games_preimage_render (preimage_card_theme->slot_preimage,
                                       preimage_card_theme->card_size.width,
                                       preimage_card_theme->card_size.height);

    return subpixbuf;
  }

  suit = card_id / 13;
  rank = card_id % 13;

  if (!theme->source &&
      (!theme->scalable ||
       !ar_card_theme_sliced_prerender_scalable (theme)))
    return NULL;

  subpixbuf = gdk_pixbuf_new_subpixbuf (theme->source,
                                        rank *
                                        theme->subsize.width,
                                        suit *
                                        theme->subsize.height,
                                        theme->subsize.width,
                                        theme->subsize.height);
  if (theme->scalable) {
    card_pixbuf = subpixbuf;
  } else {
    card_pixbuf = gdk_pixbuf_scale_simple (subpixbuf,
                                           preimage_card_theme->card_size.width,
                                           preimage_card_theme->card_size.height,
                                           GDK_INTERP_BILINEAR);
    g_object_unref (subpixbuf);
  }

  return card_pixbuf;
}

static void
ar_card_theme_sliced_init (ArCardThemeSliced *theme)
{
  theme->subsize.width = theme->subsize.height = -1;
}

static void
ar_card_theme_sliced_finalize (GObject * object)
{
  ArCardThemeSliced *theme = AR_CARD_THEME_SLICED (object);

  theme->scalable = TRUE; /* so the call to clear unrefs the source pixbuf */

  G_OBJECT_CLASS (ar_card_theme_sliced_parent_class)->finalize (object);
}

static ArCardThemeInfo *
ar_card_theme_sliced_class_get_theme_info (ArCardThemeClass *klass,
                                              const char *path,
                                              const char *filename)
{
  ArCardThemeInfo *info;
  char *name, *display_name, *pref_name;

  info = AR_CARD_THEME_CLASS (ar_card_theme_sliced_parent_class)->get_theme_info (klass, path, filename);
  if (info) {
    g_assert (info->pref_name == NULL);
    info->pref_name = g_strdup_printf ("sliced:%s", filename);
    return info;
  }

  /* This class also supports the old-style PNG format */
  if (!g_str_has_suffix (filename, ".png"))
    return NULL;

  name = games_filename_to_display_name (filename);
  display_name = g_strdup_printf ("%s (Ugly)", name);
  pref_name = g_strdup_printf ("sliced:%s", filename);
  info = _ar_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                     path,
                                     filename,
                                     display_name /* adopts */,
                                     pref_name /* adopts */,
                                     NULL, NULL);
  g_free (name);

  return info;
}

static gboolean
ar_card_theme_sliced_class_foreach_theme_dir (ArCardThemeClass *klass,
                                                 ArCardThemeForeachFunc callback,
                                                 gpointer data)
{
  char *dir;
  gboolean retval;

  if (!_ar_card_theme_class_foreach_env (klass, "AR_CARD_THEME_PATH_SLICED", callback, data))
    return FALSE;

  /* Themes in the pre-2.19 theme format: $(datadir)/pixmaps/gnome-games-common/cards */
  dir = g_build_filename (games_runtime_get_directory (GAMES_RUNTIME_DATA_DIRECTORY),
                          "pixmaps", "gnome-games-common", "cards", NULL);
  retval = callback (klass, dir, data);
  g_free (dir);

  return retval;
}

static void
ar_card_theme_sliced_class_init (ArCardThemeSlicedClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  ArCardThemeClass *theme_class = AR_CARD_THEME_CLASS (klass);
  ArCardThemePreimageClass *preimage_theme_class = AR_CARD_THEME_PREIMAGE_CLASS (klass);

  gobject_class->finalize = ar_card_theme_sliced_finalize;

  theme_class->get_theme_info = ar_card_theme_sliced_class_get_theme_info;
  theme_class->foreach_theme_dir = ar_card_theme_sliced_class_foreach_theme_dir;

  theme_class->load = ar_card_theme_sliced_load;
  theme_class->get_card_pixbuf = ar_card_theme_sliced_get_card_pixbuf;

  preimage_theme_class->needs_scalable_cards = FALSE;
  preimage_theme_class->clear_sized_theme_data = ar_card_theme_sliced_clear_sized_theme_data;
}

/* private API */

/**
 * ar_card_theme_sliced_new:
 *
 * Returns: a new #ArCardThemeSliced
 */
ArCardTheme*
ar_card_theme_sliced_new (void)
{
  return g_object_new (AR_TYPE_CARD_THEME_SLICED, NULL);
}
