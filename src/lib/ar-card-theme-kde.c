/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008, 2010 Christian Persch

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
#include <gtk/gtk.h>

#include <librsvg/rsvg.h>
#include <librsvg/rsvg-cairo.h>

#include "ar-debug.h"
#include "ar-profile.h"
#include "ar-preimage.h"
#include "ar-preimage-private.h"
#include "ar-runtime.h"
#include "ar-string-utils.h"

#include "ar-card-theme.h"
#include "ar-card-theme-private.h"

#define MAX_N_BACKS (10)

struct _ArCardThemeKDEClass {
  ArCardThemePreimageClass parent_class;

  GHashTable *back_names;
};

struct _ArCardThemeKDE {
  ArCardThemePreimage parent_instance;

  cairo_rectangle_t *card_extents;

  char *backs[MAX_N_BACKS];
  guint n_backs : 4; /* enough bits for MAX_N_BACKS */
  guint back_index : 4; /* same */
  guint has_2_jokers : 1;
  guint has_joker : 1;
};

enum {
  PROP_0,
  PROP_BACK_INDEX,
  PROP_N_BACKS,
};

#define N_ROWS ((double) 5.0)
#define N_COLS ((double) 13.0)

#define DELTA (0.0f)

#define KDE_CARDS_GROUP           "KDE Cards"
#define KDE_CARDS_NAME_KEY        "Name"
#define KDE_CARDS_SVG_KEY         "SVG"

#define KDE_BACKDECK_FILENAME     "index.desktop"
#define KDE_BACKDECK_GROUP        "KDE Backdeck"
#define KDE_BACKDECK_BACK_KEY     "Back"
#define KDE_BACKDECK_NAME_KEY     "Name" /* localised */
#define KDE_BACKDECK_PYSOL_KEY    "PySol"
#define KDE_BACKDECK_SVG_KEY      "SVG"

static gboolean
get_is_blacklisted (const char *filename)
{
  static const char *blacklist[] = {
    /* These exist in g-g-extra-data too, and ours render faster */
    "svg-dondorf",
    "svg-nicu-ornamental",
    "svg-gm-paris",
    /* These are defective, they have no back card */
    "svg-ancient-egyptians",
    "svg-future",
    "svg-jolly-royal",
    "svg-konqi-modern",
    "svg-oxygen",
    "svg-penguins",
    "svg-standard",
    "svg-xskat-french",
    /* These are broken in some other way */
    "svg-tigullio-international",
    "svg-xskat-german",
    /* These just crash librsvg / cairo */
    "svg-oxygen-white",
    /* This is a combinded theme containing card backs */
    "decks"
  };
  guint i;

  for (i = 0; i < G_N_ELEMENTS (blacklist); ++i)
    if (strcmp (filename, blacklist[i]) == 0)
      return TRUE;

  return FALSE;
}

static gboolean
ar_card_theme_kde_class_load_back_infos (ArCardThemeClass *theme_klass,
                                         const char *base_path,
                                         GHashTable *back_names)
{
  char *deck_path;
  const char *filename;
  GDir *dir;
  GKeyFile *key_file;

  deck_path = g_build_filename (base_path, "decks", NULL);
  dir = g_dir_open (deck_path, 0, NULL);
  if (dir == NULL)
    goto out;

  while ((filename = g_dir_read_name (dir)) != NULL) {
    char *path = NULL, *name = NULL, *svg_filename = NULL;

    if (!g_str_has_suffix (filename, ".desktop"))
      continue;

    path = g_build_filename (deck_path, filename, NULL);
    key_file = g_key_file_new ();
    if (!g_key_file_load_from_file (key_file, path, 0, NULL))
      goto next;

    if (!g_key_file_has_group (key_file, KDE_CARDS_GROUP))
      goto next;

    name = g_key_file_get_string (key_file, KDE_CARDS_GROUP, KDE_CARDS_NAME_KEY, NULL);
    svg_filename = g_key_file_get_string (key_file, KDE_CARDS_GROUP, KDE_CARDS_SVG_KEY, NULL);
    if (!name || !name[0] || !svg_filename || !svg_filename[0])
      goto next;

    g_hash_table_insert (back_names,
                         name /* adopts */,
                         g_build_filename (deck_path, svg_filename, NULL) /* adopts */);
    name = NULL; /* adopted above */

  next:
    g_free (path);
    g_free (name);
    g_free (svg_filename);
    g_key_file_free (key_file);

  }

  g_dir_close (dir);

out:
  g_free (deck_path);

  return TRUE;
}

static void
ar_card_theme_kde_class_ensure_back_infos (ArCardThemeKDEClass *klass)
{
  ArCardThemeClass *theme_class;

  if (klass->back_names != NULL)
    return;

  klass->back_names = g_hash_table_new_full (g_str_hash, g_str_equal,
                                             g_free, g_free);

  theme_class = AR_CARD_THEME_CLASS (klass);
  theme_class->foreach_theme_dir (theme_class,
                                  (ArCardThemeForeachFunc) ar_card_theme_kde_class_load_back_infos,
                                  klass->back_names);
}

static cairo_rectangle_t *
ar_card_theme_kde_get_card_extents (ArCardThemeKDE *theme,
                                    int card_id,
                                    const char *node)
{
  ArPreimage *preimage;
  cairo_rectangle_t *card_extents;
  cairo_rectangle_t rect;
  cairo_surface_t *surface;
  cairo_t *cr;

  card_extents = &theme->card_extents[card_id];
  /* Is it initalised yet? */
  if (card_extents->width != 0. && card_extents->height != 0.)
    return card_extents;

  preimage = ((ArCardThemePreimage *) theme)->cards_preimage;

  surface = cairo_recording_surface_create (CAIRO_CONTENT_ALPHA, NULL);
  cr = cairo_create (surface);
  ar_profilestart ("getting ink extents for node %s", node);
  rsvg_handle_render_cairo_sub (preimage->rsvg_handle, cr, node);
  ar_profileend ("getting ink extents for node %s", node);
  cairo_destroy (cr);

  cairo_recording_surface_ink_extents (surface, &rect.x, &rect.y, &rect.width, &rect.height);
  cairo_surface_destroy (surface);

  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "card %s %.3f x%.3f at (%.3f | %.3f)\n",
                      node,
                      card_extents->width, card_extents->height,
                      card_extents->x, card_extents->y);

  *card_extents = rect;

  /* Sanity check; necessary? */
  if (rect.width == 0. || rect.height == 0.)
    return NULL;

  return card_extents;
}

/* Class implementation */

G_DEFINE_TYPE (ArCardThemeKDE, ar_card_theme_kde, AR_TYPE_CARD_THEME_PREIMAGE);

static gboolean
ar_card_theme_kde_load (ArCardTheme *card_theme,
                        GError **error)
{
  static const char extra_backs[][11] = {
    "#blue_back",
    "#red_back",
    "#green_back"
  };
  ArCardThemeKDE *theme = (ArCardThemeKDE *) card_theme;
  ArPreimage *preimage;
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
  if (theme->n_backs == 0) {
    g_set_error (error, AR_CARD_THEME_ERROR, AR_CARD_THEME_ERROR_MISSING_ELEMENT,
                 "Missing element for card back");
    return FALSE;
  }

  /* Look for the jokers */
  ar_card_get_node_by_id_snprintf (node, sizeof (node), AR_CARD_BLACK_JOKER);
  has_black_joker = rsvg_handle_has_sub (preimage->rsvg_handle, node);
  ar_card_get_node_by_id_snprintf (node, sizeof (node), AR_CARD_RED_JOKER);
  has_red_joker = rsvg_handle_has_sub (preimage->rsvg_handle, node);

  has_joker = rsvg_handle_has_sub (preimage->rsvg_handle, "#joker");

  theme->has_2_jokers = has_red_joker && has_black_joker;
  theme->has_joker = has_joker;

  /* Get the card_extents of the card back, which we use to compute the theme's aspect ratio */
  if (!ar_card_theme_kde_get_card_extents (theme, AR_CARD_BACK, theme->backs[theme->back_index])) {
    g_set_error (error, AR_CARD_THEME_ERROR, AR_CARD_THEME_ERROR_GENERIC,
                 "Failed to get the theme's aspect ratio");
    return FALSE;
  }

  return TRUE;
}

static double
ar_card_theme_kde_get_card_aspect (ArCardTheme* card_theme)
{
  ArCardThemeKDE *theme = (ArCardThemeKDE *) card_theme;
  cairo_rectangle_t *card_extents;

  card_extents = &theme->card_extents[AR_CARD_BACK];
  g_assert (card_extents->width != 0 && card_extents->height != 0); /* initialised */

  return card_extents->width / card_extents->height;
}

static GdkPixbuf *
ar_card_theme_kde_get_card_pixbuf (ArCardTheme *card_theme,
                                   int card_id)
{
  ArCardThemePreimage *preimage_card_theme = (ArCardThemePreimage *) card_theme;
  ArCardThemeKDE *theme = (ArCardThemeKDE *) card_theme;
  ArPreimage *preimage = preimage_card_theme->cards_preimage;
  GdkPixbuf *subpixbuf;
  double card_width, card_height;
  double width, height;
  double zoomx, zoomy;
  char node[32];
  cairo_rectangle_t *card_extents;

  if (G_UNLIKELY (card_id == AR_CARD_SLOT)) {
    subpixbuf = ar_preimage_render (preimage_card_theme->slot_preimage,
                                       preimage_card_theme->card_size.width,
                                       preimage_card_theme->card_size.height);

    return subpixbuf;
  }

  ar_card_get_node_by_id_snprintf (node, sizeof (node), card_id);

  card_extents = ar_card_theme_kde_get_card_extents (theme, card_id, node);
  if (!card_extents)
    return NULL;

  card_width = ((double) ar_preimage_get_width (preimage)) / N_COLS;
  card_height = ((double) ar_preimage_get_height (preimage)) / N_ROWS;

  width = preimage_card_theme->card_size.width;
  height = preimage_card_theme->card_size.height;

  zoomx = width / card_width;
  zoomy = height / card_height;

//   zoomx = width / card_extents->width;
//   zoomy = height / card_extents->height;

  subpixbuf = ar_preimage_render_sub (preimage,
                                         node,
                                         preimage_card_theme->card_size.width,
                                         preimage_card_theme->card_size.height,
                                         -card_extents->x, -card_extents->y,
                                         zoomx, zoomy);

  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "Returning %p\n", subpixbuf);

  return subpixbuf;
}

static void
ar_card_theme_kde_paint_card (ArCardTheme *card_theme,
                              cairo_t *cr,
                              int card_id)
{
  ArCardThemePreimage *preimage_card_theme = (ArCardThemePreimage *) card_theme;
  ArCardThemeKDE *theme = (ArCardThemeKDE *) card_theme;
  ArPreimage *preimage = preimage_card_theme->cards_preimage;
  char node[32];
  cairo_rectangle_t *card_extents;
  cairo_matrix_t matrix;

  if (G_UNLIKELY (card_id == AR_CARD_SLOT)) {
    ar_preimage_render_cairo (preimage_card_theme->slot_preimage,
                                 cr,
                                 preimage_card_theme->card_size.width,
                                 preimage_card_theme->card_size.height);
    return;
  }

  ar_card_get_node_by_id_snprintf (node, sizeof (node), card_id);

  card_extents = ar_card_theme_kde_get_card_extents (theme, card_id, node);
  if (!card_extents)
    return;

  cairo_save (cr);

  if (preimage->font_options) {
    cairo_set_antialias (cr, cairo_font_options_get_antialias (preimage->font_options));

    cairo_set_font_options (cr, preimage->font_options);
  }

  cairo_matrix_init_identity (&matrix);
  cairo_matrix_scale (&matrix,
                      preimage_card_theme->card_size.width / card_extents->width,
                      preimage_card_theme->card_size.height / card_extents->height);
  cairo_matrix_translate (&matrix, -card_extents->x, -card_extents->y);

  cairo_set_matrix (cr, &matrix);

  rsvg_handle_render_cairo_sub (preimage->rsvg_handle, cr, node);

  cairo_restore (cr);
}

static void
ar_card_theme_kde_init (ArCardThemeKDE *theme)
{
  int i;

  theme->card_extents = g_new0 (cairo_rectangle_t, AR_CARDS_TOTAL);
  for (i = 0; i < AR_CARDS_TOTAL; ++i)
    theme->card_extents[i].width = theme->card_extents[i].height = 0.;

  theme->n_backs = 0;
  theme->back_index = 0;
}

static void
ar_card_theme_kde_finalize (GObject * object)
{
  ArCardThemeKDE *theme = AR_CARD_THEME_KDE (object);
  guint i;

  g_free (theme->card_extents);

  for (i = 0; i < theme->n_backs; ++i)
    g_free (theme->backs[i]);

  G_OBJECT_CLASS (ar_card_theme_kde_parent_class)->finalize (object);
}

static ArCardThemeInfo *
ar_card_theme_kde_class_get_theme_info (ArCardThemeClass *klass,
                                        const char *path,
                                        const char *filename)
{
  ArCardThemeInfo *info = NULL;
  char *base_path = NULL, *key_file_path = NULL;
  char *back_name;
  GKeyFile *key_file = NULL;
  char *svg_filename = NULL, *name = NULL, *display_name, *pref_name;

  if (get_is_blacklisted (filename)) {
    ar_debug_print (AR_DEBUG_CARD_THEME,
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

  back_name = g_key_file_get_string (key_file, KDE_BACKDECK_GROUP, KDE_BACKDECK_BACK_KEY, NULL);

  display_name = g_strdup_printf ("%s (KDE)", name);
  pref_name = g_strdup_printf ("kde:%s", filename);
  info = _ar_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                  base_path,
                                  svg_filename,
                                  display_name /* adopts */,
                                  pref_name /* adopts */,
                                  TRUE /* scalable */,
                                  back_name, g_free);

out:
  g_free (base_path);
  g_free (key_file_path);
  g_free (name);
  g_free (svg_filename);
  if (key_file) {
    g_key_file_free (key_file);
  }

  return info;
}

static gboolean
ar_card_theme_kde_class_foreach_theme_dir (ArCardThemeClass *klass,
                                              ArCardThemeForeachFunc callback,
                                              gpointer data)
{
  if (!_ar_card_theme_class_foreach_env (klass, "AR_CARD_THEME_PATH_KDE", callback, data))
    return FALSE;

  return callback (klass, KDE_CARD_THEME_PATH, data);
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
      theme->card_extents[AR_CARD_BACK].width = theme->card_extents[AR_CARD_BACK].height = 0;
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

  gobject_class->finalize = ar_card_theme_kde_finalize;
  gobject_class->get_property = ar_card_theme_kde_get_property;
  gobject_class->set_property = ar_card_theme_kde_set_property;

  theme_class->get_theme_info = ar_card_theme_kde_class_get_theme_info;
  theme_class->foreach_theme_dir = ar_card_theme_kde_class_foreach_theme_dir;

  theme_class->load = ar_card_theme_kde_load;
  theme_class->get_card_aspect = ar_card_theme_kde_get_card_aspect;
  theme_class->get_card_pixbuf = ar_card_theme_kde_get_card_pixbuf;
  theme_class->paint_card = ar_card_theme_kde_paint_card;

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
