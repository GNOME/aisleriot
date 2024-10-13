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

#include "ar-debug.h"
#include "ar-profile.h"
#include "ar-runtime.h"

#include "ar-card-theme.h"
#include "ar-card-theme-private.h"

enum {
  PROP_0,
  PROP_THEME_INFO
};

enum {
  CHANGED,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

/* Class implementation */

G_DEFINE_ABSTRACT_TYPE (ArCardTheme, ar_card_theme, G_TYPE_OBJECT);

static void
ar_card_theme_init (ArCardTheme * theme)
{
  theme->requires_image_surface = FALSE;
}

static GObject *
ar_card_theme_constructor (GType type,
                              guint n_construct_properties,
                              GObjectConstructParam *construct_params)
{
  GObject *object;
  ArCardTheme *theme;

  object = G_OBJECT_CLASS (ar_card_theme_parent_class)->constructor
             (type, n_construct_properties, construct_params);

  theme = AR_CARD_THEME (object);

  g_assert (theme->theme_info != NULL);

  /* NOTE! We have to do this here, since it returns the wrong class
   * (ArCardThemeClass) when called in ar_card_theme_init() !
   */
  theme->klass = AR_CARD_THEME_GET_CLASS (theme);

  return object;
}

static void
ar_card_theme_finalize (GObject *object)
{
  ArCardTheme *theme = AR_CARD_THEME (object);

  ar_card_theme_info_unref (theme->theme_info);

  G_OBJECT_CLASS (ar_card_theme_parent_class)->finalize (object);
}

static void
ar_card_theme_set_property (GObject * object,
                               guint prop_id,
                               const GValue * value, GParamSpec * pspec)
{
  ArCardTheme *theme = AR_CARD_THEME (object);

  switch (prop_id) {
    case PROP_THEME_INFO:
      theme->theme_info = g_value_dup_boxed (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static ArCardThemeInfo *
ar_card_theme_class_get_theme_info (ArCardThemeClass *klass,
                                       const char *dir,
                                       const char *filename)
{
  return NULL;
}

/* This routine is copied from librsvg:
   Copyright © 2005 Dom Lachowicz <cinamod@hotmail.com>
   Copyright © 2005 Caleb Moore <c.moore@student.unsw.edu.au>
   Copyright © 2005 Red Hat, Inc.
 */
static void
cairo_pixels_to_pixbuf (guint8 *pixels,
                        int rowstride,
                        int height)
{
  int row;

  /* un-premultiply data */
  for (row = 0; row < height; row++) {
    guint8 *row_data = (pixels + (row * rowstride));
    int i;

    for (i = 0; i < rowstride; i += 4) {
      guint8 *b = &row_data[i];
      guint32 pixel;
      guint8 alpha;

      memcpy (&pixel, b, sizeof (guint32));
      alpha = (pixel & 0xff000000) >> 24;
      if (alpha == 0) {
        b[0] = b[1] = b[2] = b[3] = 0;
      } else {
        b[0] = (((pixel & 0xff0000) >> 16) * 255 + alpha / 2) / alpha;
        b[1] = (((pixel & 0x00ff00) >> 8) * 255 + alpha / 2) / alpha;
        b[2] = (((pixel & 0x0000ff) >> 0) * 255 + alpha / 2) / alpha;
        b[3] = alpha;
      }
    }
  }
}

/* Two-way adapters cairo <-> pixbuf, so theme classes only need to implement one of them. */

static void ar_card_theme_class_real_paint_card (ArCardTheme *theme,
                                                 cairo_surface_t *surface,
                                                 int cardid);
static GdkPixbuf * ar_card_theme_class_real_get_card_pixbuf (ArCardTheme *card_theme,
                                                             int card_id);

static void
ar_card_theme_class_real_paint_card (ArCardTheme *theme,
                                     cairo_surface_t *surface,
                                     int cardid)
{
  GdkPixbuf *pixbuf;
  cairo_t *cr;

  g_assert (AR_CARD_THEME_GET_CLASS (theme)->get_card_pixbuf != ar_card_theme_class_real_get_card_pixbuf);

  pixbuf = ar_card_theme_get_card_pixbuf (theme, cardid);
  if (pixbuf == NULL)
    return;

  cr = cairo_create (surface);
  gdk_cairo_set_source_pixbuf (cr, pixbuf, 0, 0);
  cairo_paint (cr);

  cairo_destroy (cr);
  g_object_unref (pixbuf);
}

static void
card_pixbuf_destroy (guchar   *pixels,
                     gpointer  data)
{
  g_free (pixels);
}


static GdkPixbuf *
ar_card_theme_class_real_get_card_pixbuf (ArCardTheme *card_theme,
                                          int card_id)
{
  int rowstride;
  guint8 *data;
  cairo_surface_t *surface;
  CardSize card_size;

  g_assert (AR_CARD_THEME_GET_CLASS (card_theme)->paint_card != ar_card_theme_class_real_paint_card);

  ar_card_theme_get_size (card_theme, &card_size);

  rowstride = cairo_format_stride_for_width (CAIRO_FORMAT_ARGB32, card_size.width);

  data = g_try_malloc0 (rowstride * card_size.height);
  if (!data)
    return NULL;

  surface = cairo_image_surface_create_for_data (data,
                                                 CAIRO_FORMAT_ARGB32,
                                                 card_size.width, card_size.height,
                                                 rowstride);
  ar_card_theme_paint_card (card_theme, surface, card_id);

  cairo_surface_destroy (surface);
  cairo_pixels_to_pixbuf (data, rowstride, card_size.height);

  return gdk_pixbuf_new_from_data (data,
                                   GDK_COLORSPACE_RGB,
                                   TRUE,
                                   8,
                                   card_size.width, card_size.height,
                                   rowstride,
                                   card_pixbuf_destroy, NULL);
}

static void
ar_card_theme_class_init (ArCardThemeClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->set_property = ar_card_theme_set_property;
  gobject_class->constructor = ar_card_theme_constructor;
  gobject_class->finalize = ar_card_theme_finalize;

  klass->get_theme_info = ar_card_theme_class_get_theme_info;
  klass->paint_card = ar_card_theme_class_real_paint_card;
  klass->get_card_pixbuf = ar_card_theme_class_real_get_card_pixbuf;

  g_object_class_install_property
    (gobject_class,
     PROP_THEME_INFO,
     g_param_spec_boxed ("theme-info", NULL, NULL,
                         AR_TYPE_CARD_THEME_INFO,
                         G_PARAM_WRITABLE |
                         G_PARAM_STATIC_NAME |
                         G_PARAM_STATIC_NICK |
                         G_PARAM_STATIC_BLURB |
                         G_PARAM_CONSTRUCT_ONLY));

  /**
   * ArCardTheme:changed:
   * @theme: the object on which the signal is emitted
   *
   * The ::changed signal is emitted when the card theme has
   * changed in any way that makes it necessary to re-render
   * any displayed or cached images.
   */
  signals[CHANGED] =
    g_signal_newv ("changed",
                   G_TYPE_FROM_CLASS (klass),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__VOID,
                   G_TYPE_NONE,
                   0, NULL);
}

/* private API */

void
_ar_card_theme_emit_changed (ArCardTheme *theme)
{
  g_signal_emit (theme, signals[CHANGED], 0);
}

gboolean
_ar_card_theme_requires_image_surface (ArCardTheme *theme)
{
  return theme->requires_image_surface;
}

ArCardThemeInfo *
_ar_card_theme_class_get_theme_info (ArCardThemeClass *klass,
                                        const char *dir,
                                        const char *filename)
{
  return klass->get_theme_info (klass, dir, filename);
}

gboolean
_ar_card_theme_class_foreach_theme_dir (ArCardThemeClass *klass,
                                           ArCardThemeForeachFunc callback,
                                           gpointer data)
{
  return klass->foreach_theme_dir (klass, callback, data);
}

gboolean
_ar_card_theme_class_foreach_env (ArCardThemeClass *klass,
                                     const char *env,
                                     ArCardThemeForeachFunc callback,
                                     gpointer data)
{
  const char *value;
  char **paths;
  guint i;
  gboolean retval = TRUE;

  value = g_getenv (env);
  if (!value || !value[0])
    return TRUE;

  paths = g_strsplit (value, G_SEARCHPATH_SEPARATOR_S, -1);
  if (!paths)
    return TRUE;

  for (i = 0; paths[i]; ++i) {
    const char *path = paths[i];

    if (!paths[0])
      continue;

    retval = callback (klass, path, data);
    if (!retval)
      break;
  }

  g_strfreev (paths);

  return retval;
}

gboolean
_ar_card_theme_class_foreach_user_dir (ArCardThemeClass *klass,
				       const char *name,
				       ArCardThemeForeachFunc callback,
				       gpointer data)
{
  const char *udd = g_get_user_data_dir();
  if (!udd)
    return TRUE;

  char *path = g_build_filename(udd, "aisleriot", name, NULL);
  gboolean rv = callback(klass, path, data);
  g_free(path);

  return rv;
}

/* public API */

/**
 * ar_card_theme_set_font_options:
 * @theme:
 * @font_options: the #cairo_font_options_t to use
 *
 * Sets the font options to use when drawing the card images.
 */
void
ar_card_theme_set_font_options (ArCardTheme *theme,
                                   const cairo_font_options_t *font_options)
{
  g_return_if_fail (AR_IS_CARD_THEME (theme));

  if (!theme->klass->set_font_options)
    return;

  theme->klass->set_font_options (theme, font_options);
}

/**
 * ar_card_theme_get_theme_info:
 * @theme:
 *
 * Returns: the #ArCardThemeInfo corresponding to @theme.
 */
ArCardThemeInfo *
ar_card_theme_get_theme_info (ArCardTheme *theme)
{
  g_return_val_if_fail (AR_IS_CARD_THEME (theme), NULL);

  return theme->theme_info;
}

/**
 * ar_card_theme_set_size:
 * @theme:
 * @width: the maximum width
 * @height: the maximum height
 * @proportion: how much of @width and @height to use for the cards
 *
 * Calculates the card size to use. The passed-in dimensions are not used
 * directly; instead the width and height used are calculated using the
 * card theme's aspect ratio and, if using a prerendered card theme, from the
 * available sizes. @theme.card_size will contain the card size afterwards.
 * If the card size was changed, the cards cache will be cleared.
 *
 * Returns: %TRUE iff the card size was changed
 */
gboolean
ar_card_theme_set_size (ArCardTheme *theme,
                           int width,
                           int height,
                           double proportion)
{
  g_return_val_if_fail (AR_IS_CARD_THEME (theme), FALSE);

  return theme->klass->set_card_size (theme, width, height, proportion);
}

/**
 * ar_card_theme_get_size:
 * @theme:
 * @size: location to store the card size
 *
 * Returns the currently selected card size in @size.
 */
void
ar_card_theme_get_size (ArCardTheme *theme,
                           CardSize *size)
{
  theme->klass->get_card_size (theme, size);
}

/**
 * ar_card_theme_get_aspect:
 * @theme:
 *
 * Returns: the aspect ratio of the cards in the currently loaded theme
 */
double
ar_card_theme_get_aspect (ArCardTheme * theme)
{
  g_return_val_if_fail (AR_IS_CARD_THEME (theme), 1.0);

  return theme->klass->get_card_aspect (theme);
}

/**
 * ar_card_theme_get_card_pixbuf:
 * @theme:
 * @card_id:
 *
 * Returns a #GdkPixbuf for the selected card using the currently loaded
 * theme and the currently selected size.
 *
 * Returns: a new #GdkPixbuf, or %NULL if there was an error
 */
GdkPixbuf *
ar_card_theme_get_card_pixbuf (ArCardTheme *theme,
                                  int card_id)
{
  GdkPixbuf *pixbuf;

  g_return_val_if_fail ((card_id >= 0) && (card_id < AR_CARDS_TOTAL), NULL);

  ar_profilestart ("loading card %d from theme %s", card_id, theme->theme_info->display_name);

  pixbuf = theme->klass->get_card_pixbuf (theme, card_id);

  ar_profileend ("loading card %d from theme %s", card_id, theme->theme_info->display_name);

  return pixbuf;
}

/**
 * ar_card_theme_paint_card:
 * @theme:
 * @surface:
 * @card_id:
 *
 * Paints the card to @cr.
*/
void
ar_card_theme_paint_card (ArCardTheme *theme,
                          cairo_surface_t *surface,
                          int cardid)
{
  g_return_if_fail ((cardid >= 0) && (cardid < AR_CARDS_TOTAL));

  ar_profilestart ("loading card %d from theme %s", cardid, theme->theme_info->display_name);

  theme->klass->paint_card (theme, surface, cardid);

  ar_profileend ("loading card %d from theme %s", cardid, theme->theme_info->display_name);
}

/* ArCardThemeInfo impl */

static int
theme_type_compare (GType a,
                    GType b)
{
  const GType types[] = {
  /* List of supported theme types, in order of decreasing precedence */
#ifdef HAVE_RSVG
#ifdef ENABLE_CARD_THEME_FORMAT_SVG
  AR_TYPE_CARD_THEME_SVG,
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_KDE
  AR_TYPE_CARD_THEME_KDE,
#endif
#endif /* HAVE_RSVG */
#ifdef ENABLE_CARD_THEME_FORMAT_PYSOL
  AR_TYPE_CARD_THEME_PYSOL,
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_FIXED
  AR_TYPE_CARD_THEME_FIXED
#endif
  };
  guint ia, ib;

  for (ia = 0; ia < G_N_ELEMENTS (types); ++ia)
    if (types[ia] == a)
      break;
  for (ib = 0; ib < G_N_ELEMENTS (types); ++ib)
    if (types[ib] == b)
      break;

  return ia - ib;
}

/* private API */

/**
 * _ar_card_theme_info_new:
 * @type:
 * @path:
 * @filename:
 * @diplay_name:
 * @pref_name:
 * @scalable:
 * @data:
 * @destroy_notify:
 *
 * Returns: a new #ArCardThemeInfo with refcount 1
 */
ArCardThemeInfo *
_ar_card_theme_info_new (GType type,
                            const char *path,
                            const char *filename,
                            char *display_name /* adopts */,
                            char *pref_name /* adopts */,
                            gboolean scalable,
                            gpointer data /* adoptes */,
                            GDestroyNotify destroy_notify)
{
  ArCardThemeInfo *info;

  info = g_slice_new (ArCardThemeInfo);
  info->ref_count = 1;
  info->type = type;
  info->path = g_strdup (path);
  info->filename = g_strdup (filename);
  info->display_name = display_name;
  info->pref_name = pref_name;
  info->scalable = scalable != FALSE;
  info->data = data;
  info->destroy_notify = destroy_notify;

  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "Created ArCardThemeInfo for type=%s path=%s filename=%s display-name=%s\n",
                      g_type_name (type), path, filename, display_name);

  return info;
}

guint
_ar_card_theme_info_hash (const ArCardThemeInfo *a)
{
  return g_int_hash (&a->type) ^
         g_str_hash (a->path) ^
         g_str_hash (a->filename);
}

/**
 * _ar_card_theme_info_collate:
 * @a:
 * @b:
 *
 * Compares @a and @b.
 *
 * Returns: %-1 if @a comes before @b, %1 if @b comes before @a, or
 * %0 if @a and @b are equal.
 */
int
_ar_card_theme_info_collate (const ArCardThemeInfo *a,
                                const ArCardThemeInfo *b)
{
  g_return_val_if_fail (a != NULL && b != NULL, 0);

  if (a->type != b->type)
    return theme_type_compare (a->type, b->type);

  return g_utf8_collate (a->display_name, b->display_name);
}

/* public API */

G_DEFINE_BOXED_TYPE (ArCardThemeInfo, ar_card_theme_info,
                     ar_card_theme_info_ref,
                     ar_card_theme_info_unref);

/**
 * ar_card_theme_info_ref:
 * @info:
 *
 * Refs @info.
 *
 * Returns: @info
 */
ArCardThemeInfo *
ar_card_theme_info_ref (ArCardThemeInfo *info)
{
  g_return_val_if_fail (info != NULL, NULL);

  info->ref_count++;
  return info;
}

/**
 * ar_card_theme_info_unref:
 * @info:
 *
 * Unrefs @info. If the refcount reaches %0, @info is freed.
 */
void
ar_card_theme_info_unref (ArCardThemeInfo *info)
{
  g_return_if_fail (info != NULL);

  if (--info->ref_count > 0)
    return;

  g_free (info->path);
  g_free (info->filename);
  g_free (info->display_name);
  g_free (info->pref_name);

  if (info->data && info->destroy_notify)
    info->destroy_notify (info->data);

  g_slice_free (ArCardThemeInfo, info);
}

/**
 * ar_card_theme_info_get_display_name :
 * @info:
 *
 * Returns: the user readable name of @info
 */
const char *
ar_card_theme_info_get_display_name (ArCardThemeInfo *info)
{
  g_return_val_if_fail (info != NULL, NULL);

  return info->display_name;
}

/**
 * ar_card_theme_info_get_persistent_name :
 * @info:
 *
 * Returns: the user readable name of @info
 */
const char *
ar_card_theme_info_get_persistent_name (ArCardThemeInfo *info)
{
  g_return_val_if_fail (info != NULL, NULL);

  g_return_val_if_fail (info->pref_name, NULL);

  return info->pref_name;
}

/**
 * ar_card_theme_info_equal:
 *
 * Returns: %TRUE iff @a and @b refer to the same card theme
 */
gboolean
ar_card_theme_info_equal (const ArCardThemeInfo *a,
                              const ArCardThemeInfo *b)
{
  g_return_val_if_fail (a != NULL && b != NULL, FALSE);

  return a->type == b->type &&
         strcmp (a->path, b->path) == 0 &&
         strcmp (a->filename, b->filename) == 0;
}
