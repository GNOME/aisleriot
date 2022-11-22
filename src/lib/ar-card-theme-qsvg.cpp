/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008, 2010, 2014 Christian Persch

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

#include "ar-debug.h"
#include "ar-profile.h"
#include "ar-runtime.h"
#include "ar-string-utils.h"

#include "ar-card-theme.h"
#include "ar-card-theme-private.h"
#include "ar-card-theme-qsvg-private.h"

enum {
  PROP_0,
  PROP_BACK_INDEX,
  PROP_N_BACKS,
};

/* Class implementation */

extern "C" {
G_DEFINE_TYPE (ArCardThemeQSvg, ar_card_theme_qsvg, AR_TYPE_CARD_THEME);
}

static gboolean
ar_card_theme_qsvg_load (ArCardTheme *card_theme,
                        GError **error)
{
  static const char extra_backs[][12] = {
    "#blue_back",
    "#red_back",
    "#green_back"
  };
  ArCardThemeQSvg *theme = (ArCardThemeQSvg *) card_theme;
  char node[32];
  guint i;
  gboolean has_red_joker, has_black_joker, has_joker;
  ArCardThemeInfo *theme_info = card_theme->theme_info;
  char *path;

  /* Now the main course */
  path = g_build_filename (theme_info->path, theme_info->filename, NULL);

  theme->renderer = new QSvgRenderer (QString::fromUtf8 (path));
  if (!theme->renderer->isValid()) {
    g_set_error (error, AR_CARD_THEME_ERROR, AR_CARD_THEME_ERROR_GENERIC,
                 "Failed to load \"%s\"", path);
    g_free (path);
    return FALSE;
  }

  g_free (path);

  GError *err = NULL; 
  GBytes *bytes = g_resources_lookup_data("/org/gnome/aisleriot/art/slot.svg", GResourceLookupFlags(0), &err);
  g_assert_no_error (err);

  theme->slot_renderer = new QSvgRenderer(QByteArray::fromRawData((const char*)g_bytes_get_data(bytes, NULL), g_bytes_get_size(bytes)));
  g_bytes_unref(bytes);
  if (!theme->slot_renderer->isValid()) {
    g_set_error_literal (error, AR_CARD_THEME_ERROR, AR_CARD_THEME_ERROR_GENERIC,
                         "Failed to load slot.svg");
    return FALSE;
  }
  
  /* Check available backs */
  g_assert (theme->n_backs == 0);

  ar_card_get_node_by_id_snprintf (node, sizeof (node), AR_CARD_BACK);
  if (theme->renderer->elementExists(QString (node+1))) {
    theme->backs[theme->n_backs++] = g_strdup (node+1);
  }

  for (i = 0; i < G_N_ELEMENTS (extra_backs); ++i) {
    if (theme->renderer->elementExists(QString(extra_backs[i]+1))) {
      theme->backs[theme->n_backs++] = g_strdup (extra_backs[i]+1);
    }
  }

  for (i = 1; i < 10; ++i) {
    g_snprintf (node, sizeof (node), "#back_c%d", i);
    if (theme->renderer->elementExists(QString(node+1))) {
      theme->backs[theme->n_backs++] = g_strdup (node+1);
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
  has_black_joker = theme->renderer->elementExists(QString(node+1));
  ar_card_get_node_by_id_snprintf (node, sizeof (node), AR_CARD_RED_JOKER);
  has_red_joker = theme->renderer->elementExists(QString(node+1));

  has_joker = theme->renderer->elementExists(QString("joker"));

  theme->has_2_jokers = has_red_joker && has_black_joker;
  theme->has_joker = has_joker;

  ar_card_get_legacy_node_by_suit_and_rank_snprintf (node, sizeof (node), AR_CARDS_CLUBS, AR_CARD_ACE);
  theme->legacy = theme->renderer->elementExists(QString(node+1));

  /* Get the card_extents of the card back, which we use to compute the theme's aspect ratio */
  QRectF rect = theme->renderer->boundsOnElement(QString(theme->backs[theme->back_index]));
  if (rect.isNull()) {
    g_set_error (error, AR_CARD_THEME_ERROR, AR_CARD_THEME_ERROR_GENERIC,
                 "Failed to get the theme's aspect ratio");
    return FALSE;
  }

  theme->back_rect = rect;
  return TRUE;
}

static gboolean
ar_card_theme_qsvg_set_card_size (ArCardTheme *card_theme,
                                 int width,
                                 int height,
                                 double proportion)
{
  ArCardThemeQSvg *theme = (ArCardThemeQSvg *) card_theme;
  double aspect_ratio, twidth, theight;
  QSize size(width, height);

  if (theme->card_size == size)
    return FALSE;

  /* Now calculate the card size: find the maximum size that fits
   * into the given area, preserving the card's aspect ratio.
   */
  aspect_ratio = ar_card_theme_get_aspect (card_theme);

  twidth = proportion * width;
  theight = proportion * height;
  if (twidth / theight < aspect_ratio) {
    theight = twidth / aspect_ratio;
  } else {
    twidth = theight * aspect_ratio;
  }
  
  QSize tsize(twidth, theight);
  if (theme->card_size == tsize)
    return FALSE;

  theme->card_size = tsize;

  _ar_card_theme_emit_changed (card_theme);
  return TRUE;
}

static void
ar_card_theme_qsvg_get_card_size (ArCardTheme *card_theme,
                                  CardSize *size)
{
  ArCardThemeQSvg *theme = (ArCardThemeQSvg *) card_theme;

  size->width = theme->card_size.width();
  size->height = theme->card_size.height();
}

static double
ar_card_theme_qsvg_get_card_aspect (ArCardTheme* card_theme)
{
  ArCardThemeQSvg *theme = (ArCardThemeQSvg *) card_theme;
  const QRectF &rect = theme->back_rect;

  return rect.isNull() ? 1. : rect.width() / rect.height ();
}

static void
ar_card_theme_qsvg_paint_card (ArCardTheme *card_theme,
                               cairo_surface_t *surface,
                               int card_id)
{
  ArCardThemeQSvg *theme = (ArCardThemeQSvg *) card_theme;
  char node[32];

  g_assert_cmpint (cairo_surface_get_type (surface), ==, CAIRO_SURFACE_TYPE_IMAGE);
  g_assert_cmpint (cairo_image_surface_get_format (surface), ==, CAIRO_FORMAT_ARGB32);

  if (theme->legacy)
    ar_card_get_legacy_node_by_id_snprintf (node, sizeof (node), card_id);
  else
    ar_card_get_node_by_id_snprintf (node, sizeof (node), card_id);

  if (card_id != AR_CARD_SLOT &&
      !theme->renderer->elementExists(QString(node+1))) {
    return;
  }

  cairo_surface_flush(surface);

  QImage img(cairo_image_surface_get_data(surface),
             cairo_image_surface_get_width(surface),
             cairo_image_surface_get_height(surface),
             QImage::Format_ARGB32_Premultiplied);
  g_assert_cmpuint(img.bytesPerLine(), ==, cairo_image_surface_get_stride(surface));

  /* img.fill(Qt::transparent); */
  QPainter painter(&img);

  if (card_id != AR_CARD_SLOT)
    theme->renderer->render(&painter, QString(node+1));
  else
    theme->slot_renderer->render(&painter);

  cairo_surface_mark_dirty(surface);
}

static void
ar_card_theme_qsvg_init (ArCardThemeQSvg *theme)
{
  ArCardTheme *card_theme = (ArCardTheme *) theme;
  ArCardThemeQSvgClass *klass = AR_CARD_THEME_QSVG_GET_CLASS(theme);
  static char *argv[] = { (char*)"aisleriot", NULL };
  static int argc = 1;

  /* guiEnabled = true doesn't work, seems to drag in gtk+2.0 somehow. That
   * also means we have to use image surfaces.
   */
  if (!klass->app)
    klass->app = new QApplication(argc, argv, false);

  card_theme->requires_image_surface = TRUE;

  theme->back_rect = QRectF();
  theme->card_size = QSize();
  theme->n_backs = 0;
  theme->back_index = 0;
}

static void
ar_card_theme_qsvg_finalize (GObject * object)
{
  ArCardThemeQSvg *theme = AR_CARD_THEME_QSVG (object);
  guint i;

  for (i = 0; i < theme->n_backs; ++i)
    g_free (theme->backs[i]);

  if (theme->renderer)
    delete theme->renderer;
  if (theme->slot_renderer)
    delete theme->slot_renderer;

  G_OBJECT_CLASS (ar_card_theme_qsvg_parent_class)->finalize (object);
}

static void
ar_card_theme_qsvg_get_property (GObject    *object,
                                guint       property_id,
                                GValue     *value,
                                GParamSpec *pspec)
{
  ArCardThemeQSvg *theme = AR_CARD_THEME_QSVG (object);

  switch (property_id) {
    case PROP_BACK_INDEX:
      g_value_set_int (value, theme->back_index);
      //      theme->card_extents[AR_CARD_BACK].width = theme->card_extents[AR_CARD_BACK].height = 0;
      break;

    case PROP_N_BACKS:
      g_value_set_int (value, theme->n_backs);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_card_theme_qsvg_set_property (GObject      *object,
                                guint         property_id,
                                const GValue *value,
                                GParamSpec   *pspec)
{
  ArCardThemeQSvg *theme = AR_CARD_THEME_QSVG (object);

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
ar_card_theme_qsvg_class_init (ArCardThemeQSvgClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  ArCardThemeClass *theme_class = AR_CARD_THEME_CLASS (klass);

  gobject_class->finalize = ar_card_theme_qsvg_finalize;
  gobject_class->get_property = ar_card_theme_qsvg_get_property;
  gobject_class->set_property = ar_card_theme_qsvg_set_property;

  theme_class->load = ar_card_theme_qsvg_load;
  theme_class->set_card_size = ar_card_theme_qsvg_set_card_size;
  theme_class->get_card_size = ar_card_theme_qsvg_get_card_size;
  theme_class->get_card_aspect = ar_card_theme_qsvg_get_card_aspect;
  theme_class->paint_card = ar_card_theme_qsvg_paint_card;

  g_object_class_install_property
    (gobject_class,
     PROP_BACK_INDEX,
     g_param_spec_int ("back-index", NULL, NULL,
                       0, 15, 0,
                       GParamFlags(G_PARAM_READWRITE |
                                   G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property
    (gobject_class,
     PROP_N_BACKS,
     g_param_spec_int ("n-backs", NULL, NULL,
                       1, 16, 1,
                       GParamFlags(G_PARAM_READABLE |
                                   G_PARAM_STATIC_STRINGS)));
}

/* private API */

/**
 * ar_card_theme_qsvg_new:
 *
 * Returns: a new #ArCardThemeQSvg
 */
ArCardTheme*
ar_card_theme_qsvg_new (void)
{
  return (ArCardTheme *) g_object_new (AR_TYPE_CARD_THEME_QSVG, NULL);
}
