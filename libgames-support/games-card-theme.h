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

CardSize games_card_theme_get_size (GamesCardTheme * theme);

double games_card_theme_get_aspect (GamesCardTheme * theme);

GdkPixbuf *games_card_theme_get_card_pixbuf (GamesCardTheme * theme,
                                             int cardid);

/* Utility functions */

GamesCardTheme *games_card_theme_get (GamesCardThemeInfo *info);

GamesCardTheme *games_card_theme_get_by_name (const char *theme_name);

GamesCardTheme *games_card_theme_get_any (void);

GList *games_card_theme_get_all (void);

G_END_DECLS

#endif /* GAMES_CARD_THEME_H */
