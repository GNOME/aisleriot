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

#ifndef GAMES_CARD_THEME_H
#define GAMES_CARD_THEME_H

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include "games-card.h"
#include "games-preimage.h"

G_BEGIN_DECLS

#define GAMES_CARD_THEME_ERROR  (g_quark_from_static_string ("games-card-theme"))

typedef enum {
  GAMES_CARD_THEME_ERROR_GENERIC = 0
} GamesCardThemeError;

/* GamesCardTheme (abstract) */

#define GAMES_TYPE_CARD_THEME            (games_card_theme_get_type ())
#define GAMES_CARD_THEME(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME, GamesCardTheme))
#define GAMES_CARD_THEME_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME, GamesCardThemeClass))
#define GAMES_IS_CARD_THEME(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME))
#define GAMES_IS_CARD_THEME_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME))
#define GAMES_CARD_THEME_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME, GamesCardThemeClass))

typedef struct {
  gint width;
  gint height;
} CardSize;

typedef struct _GamesCardThemeClass GamesCardThemeClass;
typedef struct _GamesCardTheme      GamesCardTheme;

GType games_card_theme_get_type (void);

const char * games_card_theme_get_default_theme_path (GamesCardThemeClass *klass);

#if GTK_CHECK_VERSION (2, 10, 0)
void games_card_theme_set_font_options (GamesCardTheme *theme,
                                        const cairo_font_options_t *font_options);
#endif

gboolean games_card_theme_set_theme (GamesCardTheme *theme,
                                     const char *theme_dir,
                                     const char *theme_name);

const gchar *games_card_theme_get_theme (GamesCardTheme * theme);

gboolean games_card_theme_set_size (GamesCardTheme * theme,
                                    int width,
                                    int height,
                                    double proportion);

CardSize games_card_theme_get_size (GamesCardTheme * theme);

double games_card_theme_get_aspect (GamesCardTheme * theme);

GdkPixbuf *games_card_theme_get_card_pixbuf (GamesCardTheme * theme,
                                             int cardid);

/* GamesCardThemePreimage (abstract) */

#define GAMES_TYPE_CARD_THEME_PREIMAGE            (games_card_theme_preimage_get_type ())
#define GAMES_CARD_THEME_PREIMAGE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME_PREIMAGE, GamesCardThemePreimage))
#define GAMES_CARD_THEME_PREIMAGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME_PREIMAGE, GamesCardThemePreimageClass))
#define GAMES_IS_CARD_THEME_PREIMAGE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME_PREIMAGE))
#define GAMES_IS_CARD_THEME_PREIMAGE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME_PREIMAGE))
#define GAMES_CARD_THEME_PREIMAGE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME_PREIMAGE, GamesCardThemePreimageClass))

typedef struct _GamesCardThemePreimageClass GamesCardThemePreimageClass;
typedef struct _GamesCardThemePreimage      GamesCardThemePreimage;

GType games_card_theme_preimage_get_type (void);

/* GamesCardThemeSVG */

#define GAMES_TYPE_CARD_THEME_SVG            (games_card_theme_svg_get_type ())
#define GAMES_CARD_THEME_SVG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME_SVG, GamesCardThemeSVG))
#define GAMES_CARD_THEME_SVG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME_SVG, GamesCardThemeSVGClass))
#define GAMES_IS_CARD_THEME_SVG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME_SVG))
#define GAMES_IS_CARD_THEME_SVG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME_SVG))
#define GAMES_CARD_THEME_SVG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME_SVG, GamesCardThemeSVGClass))

typedef struct _GamesCardThemeSVGClass GamesCardThemeSVGClass;
typedef struct _GamesCardThemeSVG      GamesCardThemeSVG;

GType games_card_theme_svg_get_type (void);

GamesCardTheme* games_card_theme_svg_new (void);

/* GamesCardThemeSliced */

#define GAMES_TYPE_CARD_THEME_SLICED            (games_card_theme_sliced_get_type ())
#define GAMES_CARD_THEME_SLICED(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME_SLICED, GamesCardThemeSliced))
#define GAMES_CARD_THEME_SLICED_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME_SLICED, GamesCardThemeSlicedClass))
#define GAMES_IS_CARD_THEME_SLICED(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME_SLICED))
#define GAMES_IS_CARD_THEME_SLICED_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME_SLICED))
#define GAMES_CARD_THEME_SLICED_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME_SLICED, GamesCardThemeSlicedClass))

typedef struct _GamesCardThemeSlicedClass GamesCardThemeSlicedClass;
typedef struct _GamesCardThemeSliced      GamesCardThemeSliced;

GType games_card_theme_sliced_get_type (void);

GamesCardTheme* games_card_theme_sliced_new (void);

/* GamesCardThemeFixed */

#define GAMES_TYPE_CARD_THEME_FIXED            (games_card_theme_fixed_get_type ())
#define GAMES_CARD_THEME_FIXED(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME_FIXED, GamesCardThemeFixed))
#define GAMES_CARD_THEME_FIXED_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME_FIXED, GamesCardThemeFixedClass))
#define GAMES_IS_CARD_THEME_FIXED(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME_FIXED))
#define GAMES_IS_CARD_THEME_FIXED_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME_FIXED))
#define GAMES_CARD_THEME_FIXED_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME_FIXED, GamesCardThemeFixedClass))

typedef struct _GamesCardThemeFixedClass GamesCardThemeFixedClass;
typedef struct _GamesCardThemeFixed      GamesCardThemeFixed;

GType games_card_theme_fixed_get_type (void);

GamesCardTheme* games_card_theme_fixed_new (void);

G_END_DECLS

/* Utility functions */

GamesCardTheme *games_card_theme_new (void);

#endif /* GAMES_CARD_THEME_H */
