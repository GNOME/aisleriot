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

#define FLOAT_TO_INT_CEIL(f) ((int) (f + 0.5f))

struct _GamesCardThemeClass {
  GObjectClass parent_class;

  /* vfuncs */
  gboolean    (* load_theme)        (GamesCardTheme *theme,
                                     const char *theme_dir,
                                     const char *theme_name,
                                     GError **error);
  const char* (* get_theme_name)    (GamesCardTheme *theme);
  gboolean    (* set_card_size)     (GamesCardTheme *theme,
                                     int width,
                                     int height,
                                     double proportion);
  CardSize    (* get_card_size)     (GamesCardTheme *theme);
  double      (* get_card_aspect)   (GamesCardTheme *theme);
  GdkPixbuf*  (* get_card_pixbuf)   (GamesCardTheme *theme,
                                     int card_id);

#if GTK_CHECK_VERSION (2, 10, 0)
  void        (* set_font_options)  (GamesCardTheme *theme,
                                     const cairo_font_options_t *font_options);
#endif
};

struct _GamesCardTheme {
  GObject parent;

  GamesCardThemeClass *klass;

  char *theme_name;
};

void _games_card_theme_emit_changed (GamesCardTheme * theme);
