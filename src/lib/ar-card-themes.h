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

#ifndef AR_CARD_THEMES_H
#define AR_CARD_THEMES_H

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "ar-card-theme.h"

G_BEGIN_DECLS

#define AR_TYPE_CARD_THEMES            (ar_card_themes_get_type ())
#define AR_CARD_THEMES(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_THEMES, ArCardThemes))
#define AR_CARD_THEMES_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_THEMES, ArCardThemesClass))
#define AR_IS_CARD_THEMES(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_THEMES))
#define AR_IS_CARD_THEMES_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_THEMES))
#define AR_CARD_THEMES_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_THEMES, ArCardThemesClass))

typedef struct _ArCardThemesClass ArCardThemesClass;
typedef struct _ArCardThemes      ArCardThemes;

GType ar_card_themes_get_type (void);

ArCardThemes *ar_card_themes_new (void);

void ar_card_themes_request_themes (ArCardThemes *theme_manager);

gboolean ar_card_themes_get_themes_loaded (ArCardThemes *theme_manager);

GList *ar_card_themes_get_themes (ArCardThemes *theme_manager);

ArCardTheme *ar_card_themes_get_theme (ArCardThemes *theme_manager,
                                             ArCardThemeInfo *info);

ArCardTheme *ar_card_themes_get_theme_by_name (ArCardThemes *theme_manager,
                                                     const char *theme_name);

ArCardTheme *ar_card_themes_get_theme_any (ArCardThemes *theme_manager);

void ar_card_themes_install_themes (ArCardThemes *theme_manager,
                                    GtkWidget *parent_window,
                                    guint user_time);

G_END_DECLS

#endif /* AR_CARD_THEMES_H */
