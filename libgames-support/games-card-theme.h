/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007 Christian Persch

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
#include "games-preimage.h"

G_BEGIN_DECLS

#define GAMES_TYPE_CARD_THEME            (games_card_theme_get_type ())
#define GAMES_CARD_THEME(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME, GamesCardTheme))
#define GAMES_CARD_THEME_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME, GamesCardThemeClass))
#define GAMES_IS_CARD_THEME(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME))
#define GAMES_IS_CARD_THEME_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME))
#define GAMES_CARD_THEME_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME))

enum {
  /* Cards */
  GAMES_CARD_JOKER = 0,
  GAMES_CARD_ACE = 1,
  GAMES_CARD_TWO = 2,
  GAMES_CARD_THREE = 3,
  GAMES_CARD_FOUR = 4,
  GAMES_CARD_FIVE = 5,
  GAMES_CARD_SIX = 6,
  GAMES_CARD_SEVEN = 7,
  GAMES_CARD_EIGHT = 8,
  GAMES_CARD_NINE = 9,
  GAMES_CARD_TEN = 10,
  GAMES_CARD_JACK = 11,
  GAMES_CARD_QUEEN = 12,
  GAMES_CARD_KING = 13,
  GAMES_CARD_ACE_HIGH = 14,

  /* Suites */
  GAMES_CARDS_CLUBS = 0,
  GAMES_CARDS_DIAMONDS = 2,
  GAMES_CARDS_HEARTS = 1,
  GAMES_CARDS_SPADES = 3,

  /* Jokers */
  GAMES_CARD_BLACK_JOKER = 52,
  GAMES_CARD_RED_JOKER = 53,

  /* Special */
  GAMES_CARD_BACK = 54,
  GAMES_CARD_SLOT = 55,
  GAMES_CARDS_TOTAL = 56,
};

typedef struct {
  gint width;
  gint height;
} CardSize;

typedef struct _GamesCardTheme GamesCardTheme;
typedef GObjectClass GamesCardThemeClass;

GType games_card_theme_get_type (void);

GamesCardTheme *games_card_theme_new (const char *theme_dir,
                                      gboolean scalable);

void games_card_theme_set_antialias (GamesCardTheme * theme,
                                     guint antialias, guint subpixel_order);

gboolean games_card_theme_set_theme (GamesCardTheme * theme,
                                     const gchar * name);

const gchar *games_card_theme_get_theme (GamesCardTheme * theme);

gboolean games_card_theme_set_size (GamesCardTheme * theme,
                                    gint width,
                                    gint height, gdouble proportion);

CardSize games_card_theme_get_size (GamesCardTheme * theme);

double games_card_theme_get_aspect (GamesCardTheme * theme);

GdkPixbuf *games_card_theme_get_card_pixbuf (GamesCardTheme * theme,
                                             gint cardid);

gchar *games_card_theme_get_card_name (GamesCardTheme * theme, gint card_id);

G_END_DECLS

#endif /* GAMES_CARD_THEME_H */
