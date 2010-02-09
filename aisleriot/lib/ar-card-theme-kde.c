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

#include <libgames-support/games-debug.h>
#include <libgames-support/games-profile.h>
#include <libgames-support/games-preimage.h>
#include <libgames-support/games-preimage-private.h>
#include <libgames-support/games-runtime.h>
#include <libgames-support/games-string-utils.h>

#include "ar-card-theme.h"
#include "ar-card-theme-private.h"

#define MAX_N_BACKS (10)

typedef struct {
  gboolean initialised;
  double width;
  double height;
  double x;
  double y;
} CardBbox;

struct _ArCardThemeKDEClass {
  ArCardThemePreimageClass parent_class;
};

struct _ArCardThemeKDE {
  ArCardThemePreimage parent_instance;

  CardBbox *bboxes;

  char *backs[MAX_N_BACKS];
  guint n_backs : 4; /* enough bits for MAX_N_BACKS */
  guint back_index : 4; /* same */
  guint has_2_jokers : 1;
  guint has_joker : 1;
};

#include <librsvg/librsvg-features.h>

/* We need librsvg >= 2.22.4 here */
#if defined(LIBRSVG_CHECK_VERSION)
#if LIBRSVG_CHECK_VERSION(2, 26, 0)
#define HAVE_NEW_RSVG
#endif
#endif

enum {
  PROP_0,
  PROP_BACK_INDEX,
  PROP_N_BACKS,
};

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

static gboolean
get_is_blacklisted (const char *filename)
{
  /* Let's blacklist these themes, since they exist in g-g-extra-data too,
   * and our variants render faster.
   */
  static const char *blacklist[] = {
    "svg-dondorf",
    "svg-nicu-ornamental",
    "svg-gm-paris"
  };
  guint i;

  for (i = 0; i < G_N_ELEMENTS (blacklist); ++i)
    if (strcmp (filename, blacklist[i]) == 0)
      return TRUE;

  return FALSE;
}

#ifdef HAVE_NEW_RSVG

static CardBbox *
ar_card_theme_kde_get_card_bbox (ArCardThemeKDE *theme,
                                    int card_id,
                                    const char *node)
{
  CardBbox *bbox;
  GamesPreimage *preimage;
  RsvgDimensionData dim;
  RsvgPositionData pos;
  gboolean retval;

  bbox = &theme->bboxes[card_id];
  if (bbox->initialised)
    return bbox;

  preimage = ((ArCardThemePreimage *) theme)->cards_preimage;

  _games_profile_start ("rsvg_handle_get_dimensions_sub node %s", node);
  retval = rsvg_handle_get_dimensions_sub (preimage->rsvg_handle, &dim, node);
  _games_profile_end ("rsvg_handle_get_dimensions_sub node %s", node);

  if (!retval) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to get dim for '%s'\n", node);
    return NULL;
  }

  _games_profile_start ("rsvg_handle_get_position_sub node %s", node);
  retval = rsvg_handle_get_position_sub (preimage->rsvg_handle, &pos, node);
  _games_profile_end ("rsvg_handle_get_position_sub node %s", node);

  if (!retval) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to get pos for '%s'\n", node);
    return NULL;
  }

  /* Sanity check; necessary? */
  if (dim.width <= 0 || dim.height <= 0)
    return NULL;

  bbox->initialised = TRUE;
  bbox->width = dim.width;
  bbox->height = dim.height;
  bbox->x = pos.x;
  bbox->y = pos.y;

  _games_debug_print (GAMES_DEBUG_CARD_THEME,
                      "card %s position %.3f:%.3f dimension %.3f:%.3f\n",
                      node,
                      bbox->x, bbox->y, bbox->width, bbox->height);

  return bbox;
}

#endif /* HAVE_NEW_RSVG */

/* Class implementation */

G_DEFINE_TYPE (ArCardThemeKDE, ar_card_theme_kde, AR_TYPE_CARD_THEME_PREIMAGE);

static gboolean
ar_card_theme_kde_load (ArCardTheme *card_theme,
                           GError **error)
{
#ifdef HAVE_NEW_RSVG
  static const char extra_backs[][11] = {
    "#blue_back",
    "#red_back",
    "#green_back"
  };
  ArCardThemeKDE *theme = (ArCardThemeKDE *) card_theme;
  GamesPreimage *preimage;
  char node[32];
  guint i;
  gboolean has_red_joker, has_black_joker, has_joker;

  if (!AR_CARD_THEME_CLASS (ar_card_theme_kde_parent_class)->load (card_theme, error))
    return FALSE;

  preimage = ((ArCardThemePreimage *) theme)->cards_preimage;

  /* Check available backs */
  g_assert (theme->n_backs == 0);

  ar_card_get_node_by_id_snprintf (node, sizeof (node), AR_CARD_BACK);
  if (rsvg_handle_has_sub (preimage->rsvg_handle, node)) {
    theme->backs[theme->n_backs++] = g_strdup (node);
  }

  for (i = 0; i < G_N_ELEMENTS (extra_backs); ++i) {
    if (rsvg_handle_has_sub (preimage->rsvg_handle, extra_backs[i])) {
      theme->backs[theme->n_backs++] = g_strdup (extra_backs[i]);
    }
  }

  for (i = 1; i < 10; ++i) {
    g_snprintf (node, sizeof (node), "#back_c%d", i);
    if (rsvg_handle_has_sub (preimage->rsvg_handle, node)) {
      theme->backs[theme->n_backs++] = g_strdup (node);
    }
  }

  /* No backs at all? Fail! */
  if (theme->n_backs == 0)
    return FALSE;

  /* Look for the jokers */
  ar_card_get_node_by_id_snprintf (node, sizeof (node), AR_CARD_BLACK_JOKER);
  has_black_joker = rsvg_handle_has_sub (preimage->rsvg_handle, node);
  ar_card_get_node_by_id_snprintf (node, sizeof (node), AR_CARD_RED_JOKER);
  has_red_joker = rsvg_handle_has_sub (preimage->rsvg_handle, node);

  has_joker = rsvg_handle_has_sub (preimage->rsvg_handle, "#joker");

  theme->has_2_jokers = has_red_joker && has_black_joker;
  theme->has_joker = has_joker;
  
  /* Get the bbox of the card back, which we use to compute the theme's aspect ratio */
  if (!ar_card_theme_kde_get_card_bbox (theme, AR_CARD_BACK, theme->backs[theme->back_index])) {
    g_set_error (error, AR_CARD_THEME_ERROR, AR_CARD_THEME_ERROR_GENERIC,
                 "Failed to get the theme's aspect ratio");
    return FALSE;
  }

  return TRUE;
#else
  return FALSE;
#endif /* HAVE_NEW_RSVG */
}

static double
ar_card_theme_kde_get_card_aspect (ArCardTheme* card_theme)
{
  ArCardThemeKDE *theme = (ArCardThemeKDE *) card_theme;
  CardBbox *bbox;

  bbox = &theme->bboxes[AR_CARD_BACK];
  g_assert (bbox->initialised);

  return bbox->width / bbox->height;
}

static GdkPixbuf *
ar_card_theme_kde_get_card_pixbuf (ArCardTheme *card_theme,
                                      int card_id)
{
#ifdef HAVE_NEW_RSVG
  ArCardThemePreimage *preimage_card_theme = (ArCardThemePreimage *) card_theme;
  ArCardThemeKDE *theme = (ArCardThemeKDE *) card_theme;
  GamesPreimage *preimage = preimage_card_theme->cards_preimage;
  GdkPixbuf *subpixbuf;
  double card_width, card_height;
  double width, height;
  double zoomx, zoomy;
  char node[32];
  CardBbox *bbox;

  if (G_UNLIKELY (card_id == AR_CARD_SLOT)) {
    subpixbuf = games_preimage_render (preimage_card_theme->slot_preimage,
                                       preimage_card_theme->card_size.width,
                                       preimage_card_theme->card_size.height);

    return subpixbuf;
  }

  ar_card_get_node_by_id_snprintf (node, sizeof (node), card_id);

  bbox = ar_card_theme_kde_get_card_bbox (theme, card_id, node);
  if (!bbox)
    return NULL;

  card_width = ((double) games_preimage_get_width (preimage)) / N_COLS;
  card_height = ((double) games_preimage_get_height (preimage)) / N_ROWS;

  width = preimage_card_theme->card_size.width;
  height = preimage_card_theme->card_size.height;

  zoomx = width / card_width;
  zoomy = height / card_height;

//   zoomx = width / bbox->width;
//   zoomy = height / bbox->height;

  subpixbuf = games_preimage_render_sub (preimage,
                                         node,
                                         preimage_card_theme->card_size.width,
                                         preimage_card_theme->card_size.height,
                                         -bbox->x, -bbox->y,
                                         zoomx, zoomy);

  _games_debug_print (GAMES_DEBUG_CARD_THEME,
                      "Returning %p\n", subpixbuf);

  return subpixbuf;
#else
  return NULL;
#endif /* HAVE_NEW_RSVG */
}

static void
ar_card_theme_kde_init (ArCardThemeKDE *theme)
{
  theme->bboxes = g_new0 (CardBbox, AR_CARDS_TOTAL);

  theme->n_backs = 0;
  theme->back_index = 0;
}

static void
ar_card_theme_kde_finalize (GObject * object)
{
  ArCardThemeKDE *theme = AR_CARD_THEME_KDE (object);
  guint i;

  g_free (theme->bboxes);

  for (i = 0; i < theme->n_backs; ++i)
    g_free (theme->backs[i]);

  G_OBJECT_CLASS (ar_card_theme_kde_parent_class)->finalize (object);
}

static ArCardThemeInfo *
ar_card_theme_kde_class_get_theme_info (ArCardThemeClass *klass,
                                           const char *path,
                                           const char *filename)
{
#ifdef HAVE_NEW_RSVG
  ArCardThemeInfo *info = NULL;
  char *base_path = NULL, *key_file_path = NULL;
  GKeyFile *key_file = NULL;
  char *svg_filename = NULL, *name = NULL, *display_name, *pref_name;

  if (get_is_blacklisted (filename)) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "KDE card theme %s is blacklisted\n", filename);
    return NULL;
  }

  base_path = g_build_filename (path, filename, NULL);
  if (!g_file_test (path, G_FILE_TEST_IS_DIR))
    goto out;

  key_file_path = g_build_filename (base_path, KDE_BACKDECK_FILENAME, NULL);
  key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (key_file, key_file_path, 0, NULL))
    goto out;
      
  if (!g_key_file_has_group (key_file, KDE_BACKDECK_GROUP))
    goto out;

  name = g_key_file_get_locale_string (key_file, KDE_BACKDECK_GROUP, KDE_BACKDECK_NAME_KEY, NULL, NULL);
  svg_filename = g_key_file_get_string (key_file, KDE_BACKDECK_GROUP, KDE_BACKDECK_SVG_KEY, NULL);
  if (!name || !name[0] || !svg_filename || !svg_filename[0])
    goto out;

  display_name = g_strdup_printf ("%s (KDE)", name);
  pref_name = g_strdup_printf ("kde:%s", filename);
  info = _ar_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                     base_path,
                                     svg_filename,
                                     display_name /* adopts */,
                                     pref_name /* adopts */,
                                     NULL, NULL);

out:
  g_free (base_path);
  g_free (key_file_path);
  g_free (name);
  g_free (svg_filename);
  if (key_file) {
    g_key_file_free (key_file);
  }

  return info;
#else
  return NULL;
#endif /* HAVE_NEW_RSVG */
}

static gboolean
ar_card_theme_kde_class_foreach_theme_dir (ArCardThemeClass *klass,
                                              ArCardThemeForeachFunc callback,
                                              gpointer data)
{
  if (!_ar_card_theme_class_foreach_env (klass, "AR_CARD_THEME_PATH_KDE", callback, data))
    return FALSE;

  /* FIXMEchpe: is this universal, or ubuntu specific? */
  return callback (klass, "/usr/share/kde4/apps/carddecks", data);
}

static void
ar_card_theme_kde_get_property (GObject    *object,
                                   guint       property_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
  ArCardThemeKDE *theme = AR_CARD_THEME_KDE (object);

  switch (property_id) {
    case PROP_BACK_INDEX:
      g_value_set_int (value, theme->back_index);
      break;

    case PROP_N_BACKS:
      g_value_set_int (value, theme->n_backs);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_card_theme_kde_set_property (GObject      *object,
                                   guint         property_id,
                                   const GValue *value,
                                   GParamSpec   *pspec)
{
  ArCardThemeKDE *theme = AR_CARD_THEME_KDE (object);

  switch (property_id) {
    case PROP_BACK_INDEX:
      theme->back_index = g_value_get_int (value);
      theme->back_index = CLAMP (theme->back_index, 0, theme->n_backs);

      /* FIXMEchpe don't invalidate the whole thing, just the BACK card */
      _ar_card_theme_emit_changed (AR_CARD_THEME (theme));
      break;

    case PROP_N_BACKS:
      /* not writable */
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_card_theme_kde_class_init (ArCardThemeKDEClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  ArCardThemeClass *theme_class = AR_CARD_THEME_CLASS (klass);
  ArCardThemePreimageClass *preimage_theme_class = AR_CARD_THEME_PREIMAGE_CLASS (klass);

  gobject_class->finalize = ar_card_theme_kde_finalize;
  gobject_class->get_property = ar_card_theme_kde_get_property;
  gobject_class->set_property = ar_card_theme_kde_set_property;

  theme_class->get_theme_info = ar_card_theme_kde_class_get_theme_info;
  theme_class->foreach_theme_dir = ar_card_theme_kde_class_foreach_theme_dir;

  theme_class->load = ar_card_theme_kde_load;
  theme_class->get_card_aspect = ar_card_theme_kde_get_card_aspect;
  theme_class->get_card_pixbuf = ar_card_theme_kde_get_card_pixbuf;

  preimage_theme_class->needs_scalable_cards = TRUE;


  g_object_class_install_property
    (gobject_class,
     PROP_BACK_INDEX,
     g_param_spec_int ("back-index", NULL, NULL,
                       0, 15, 0,
                       G_PARAM_READWRITE |
                       G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (gobject_class,
     PROP_N_BACKS,
     g_param_spec_int ("n-backs", NULL, NULL,
                       1, 16, 1,
                       G_PARAM_READABLE |
                       G_PARAM_STATIC_STRINGS));
}

/* private API */

/**
 * ar_card_theme_kde_new:
 *
 * Returns: a new #ArCardThemeKDE
 */
ArCardTheme*
ar_card_theme_kde_new (void)
{
  return g_object_new (AR_TYPE_CARD_THEME_KDE, NULL);
}
