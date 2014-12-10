/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008, 2009 Christian Persch

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

#ifndef AR_CARD_THEME_H
#define AR_CARD_THEME_H

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define AR_CARD_THEME_ERROR  (g_quark_from_static_string ("games-card-theme"))

typedef enum {
  AR_CARD_THEME_ERROR_GENERIC = 0,
  AR_CARD_THEME_ERROR_NOT_SCALABLE = 1,
  AR_CARD_THEME_ERROR_MISSING_ELEMENT = 2
} ArCardThemeError;

/* ArCardThemeInfo (boxed) */

#define AR_TYPE_CARD_THEME_INFO (ar_card_theme_info_get_type ())

typedef struct _ArCardThemeInfo ArCardThemeInfo;

GType ar_card_theme_info_get_type (void);

ArCardThemeInfo *ar_card_theme_info_ref (ArCardThemeInfo *info);

void ar_card_theme_info_unref (ArCardThemeInfo *info);

const char *ar_card_theme_info_get_display_name (ArCardThemeInfo *info);

const char *ar_card_theme_info_get_persistent_name (ArCardThemeInfo *info);

gboolean ar_card_theme_info_equal (const ArCardThemeInfo *a,
                                      const ArCardThemeInfo *b);

/* ArCardTheme (abstract) */

#define AR_TYPE_CARD_THEME            (ar_card_theme_get_type ())
#define AR_CARD_THEME(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_THEME, ArCardTheme))
#define AR_CARD_THEME_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_THEME, ArCardThemeClass))
#define AR_IS_CARD_THEME(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_THEME))
#define AR_IS_CARD_THEME_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_THEME))
#define AR_CARD_THEME_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_THEME, ArCardThemeClass))

typedef struct {
  int width;
  int height;
} CardSize;

typedef struct _ArCardThemeClass ArCardThemeClass;
typedef struct _ArCardTheme      ArCardTheme;

GType ar_card_theme_get_type (void);

void ar_card_theme_set_font_options (ArCardTheme *theme,
                                        const cairo_font_options_t *font_options);

gboolean ar_card_theme_set_theme (ArCardTheme *theme,
                                     ArCardThemeInfo *info);

ArCardThemeInfo *ar_card_theme_get_theme_info (ArCardTheme * theme);

gboolean ar_card_theme_set_size (ArCardTheme * theme,
                                    int width,
                                    int height,
                                    double proportion);

void ar_card_theme_get_size (ArCardTheme *theme,
                                CardSize *size);

double ar_card_theme_get_aspect (ArCardTheme * theme);

GdkPixbuf *ar_card_theme_get_card_pixbuf (ArCardTheme * theme,
                                          int cardid);

void ar_card_theme_paint_card (ArCardTheme *theme,
                                cairo_surface_t *surface,
                                int cardid);

G_END_DECLS

#endif /* AR_CARD_THEME_H */
