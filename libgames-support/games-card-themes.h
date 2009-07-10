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

#ifndef GAMES_CARD_THEMES_H
#define GAMES_CARD_THEMES_H

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "games-card-theme.h"

G_BEGIN_DECLS

#define GAMES_TYPE_CARD_THEMES            (games_card_themes_get_type ())
#define GAMES_CARD_THEMES(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEMES, GamesCardThemes))
#define GAMES_CARD_THEMES_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEMES, GamesCardThemesClass))
#define GAMES_IS_CARD_THEMES(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEMES))
#define GAMES_IS_CARD_THEMES_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEMES))
#define GAMES_CARD_THEMES_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEMES, GamesCardThemesClass))

typedef struct _GamesCardThemesClass GamesCardThemesClass;
typedef struct _GamesCardThemes      GamesCardThemes;

GType games_card_themes_get_type (void);

GamesCardThemes *games_card_themes_new (void);

void games_card_themes_request_themes (GamesCardThemes *theme_manager);

gboolean games_card_themes_get_themes_loaded (GamesCardThemes *theme_manager);

GList *games_card_themes_get_themes (GamesCardThemes *theme_manager);

GamesCardTheme *games_card_themes_get_theme (GamesCardThemes *theme_manager,
                                             GamesCardThemeInfo *info);

GamesCardTheme *games_card_themes_get_theme_by_name (GamesCardThemes *theme_manager,
                                                     const char *theme_name);

GamesCardTheme *games_card_themes_get_theme_any (GamesCardThemes *theme_manager);

gboolean games_card_themes_can_install_themes (GamesCardThemes *theme_manager);

void games_card_themes_install_themes (GamesCardThemes *theme_manager,
                                       GtkWindow *parent_window,
                                       guint user_time);

G_END_DECLS

#endif /* GAMES_CARD_THEMES_H */
