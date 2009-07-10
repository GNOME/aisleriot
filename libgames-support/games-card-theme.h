/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008, 2009 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#ifndef GAMES_CARD_THEME_H
#define GAMES_CARD_THEME_H

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GAMES_CARD_THEME_ERROR  (g_quark_from_static_string ("games-card-theme"))

typedef enum {
  GAMES_CARD_THEME_ERROR_GENERIC = 0,
  GAMES_CARD_THEME_ERROR_NOT_SCALABLE = 1
} GamesCardThemeError;

/* GamesCardThemeInfo (boxed) */

#define GAMES_TYPE_CARD_THEME_INFO (games_card_theme_info_get_type ())

typedef struct _GamesCardThemeInfo GamesCardThemeInfo;

GType games_card_theme_info_get_type (void);

GamesCardThemeInfo *games_card_theme_info_ref (GamesCardThemeInfo *info);

void games_card_theme_info_unref (GamesCardThemeInfo *info);

const char *games_card_theme_info_get_display_name (GamesCardThemeInfo *info);

const char *games_card_theme_info_get_persistent_name (GamesCardThemeInfo *info);

gboolean games_card_theme_info_equal (const GamesCardThemeInfo *a,
                                      const GamesCardThemeInfo *b);

/* GamesCardTheme (abstract) */

#define GAMES_TYPE_CARD_THEME            (games_card_theme_get_type ())
#define GAMES_CARD_THEME(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME, GamesCardTheme))
#define GAMES_CARD_THEME_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME, GamesCardThemeClass))
#define GAMES_IS_CARD_THEME(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME))
#define GAMES_IS_CARD_THEME_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME))
#define GAMES_CARD_THEME_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME, GamesCardThemeClass))

typedef struct {
  int width;
  int height;
} CardSize;

typedef struct _GamesCardThemeClass GamesCardThemeClass;
typedef struct _GamesCardTheme      GamesCardTheme;

GType games_card_theme_get_type (void);

#if GTK_CHECK_VERSION (2, 10, 0)
void games_card_theme_set_font_options (GamesCardTheme *theme,
                                        const cairo_font_options_t *font_options);
#endif

gboolean games_card_theme_set_theme (GamesCardTheme *theme,
                                     GamesCardThemeInfo *info);

GamesCardThemeInfo *games_card_theme_get_theme_info (GamesCardTheme * theme);

gboolean games_card_theme_set_size (GamesCardTheme * theme,
                                    int width,
                                    int height,
                                    double proportion);

void games_card_theme_get_size (GamesCardTheme *theme,
                                CardSize *size);

double games_card_theme_get_aspect (GamesCardTheme * theme);

GdkPixbuf *games_card_theme_get_card_pixbuf (GamesCardTheme * theme,
                                             int cardid);

G_END_DECLS

#endif /* GAMES_CARD_THEME_H */
