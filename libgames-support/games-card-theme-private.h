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

#include "games-card.h"

#ifdef HAVE_RSVG
#include "games-preimage.h"
#endif

#define FLOAT_TO_INT_CEIL(f) ((int) (f + 0.5f))

/* GamesCardThemeInfo */

struct _GamesCardThemeInfo {
  int ref_count;
  GType type;
  char *path;
  char *filename;
  char *display_name;
  char *pref_name;

  gpointer data;
  GDestroyNotify destroy_notify;
};

GamesCardThemeInfo *_games_card_theme_info_new (GType type,
                                                const char *path,
                                                const char *filename,
                                                char *display_name /* adopts */,
                                                char *pref_name /* adopts */,
                                                gpointer data,
                                                GDestroyNotify destroy_notify);

guint _games_card_theme_info_hash  (const GamesCardThemeInfo *a);

int _games_card_theme_info_collate (const GamesCardThemeInfo *a,
                                    const GamesCardThemeInfo *b);

/* GamesCardTheme */

/* Return TRUE to continue, FALSE to abort */
typedef gboolean (* GamesCardThemeForeachFunc) (GamesCardThemeClass *klass,
                                                const char *path,
                                                gpointer data);

struct _GamesCardThemeClass {
  GObjectClass parent_class;

  /* class vfuncs */
  GamesCardThemeInfo * (* get_theme_info)     (GamesCardThemeClass *klass,
                                               const char *dir,
                                               const char *filename);
  gboolean             (* foreach_theme_dir)  (GamesCardThemeClass *klass,
                                               GamesCardThemeForeachFunc,
                                               gpointer data);

  /* vfuncs */
  gboolean    (* load)              (GamesCardTheme *theme,
                                     GError **error);
  gboolean    (* set_card_size)     (GamesCardTheme *theme,
                                     int width,
                                     int height,
                                     double proportion);
  void        (* get_card_size)     (GamesCardTheme *theme,
                                     CardSize *size);
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

  GamesCardThemeInfo *theme_info;
};

void _games_card_theme_class_get_theme_infos (GamesCardThemeClass *klass,
                                              GList **list);
GamesCardThemeInfo *_games_card_theme_class_get_theme_info (GamesCardThemeClass *klass,
                                                            const char *dir,
                                                            const char *filename);

gboolean _games_card_theme_class_foreach_theme_dir (GamesCardThemeClass *klass,
                                                    GamesCardThemeForeachFunc callback,
                                                    gpointer data);
gboolean _games_card_theme_class_foreach_env (GamesCardThemeClass *klass,
                                              const char *env,
                                              GamesCardThemeForeachFunc callback,
                                              gpointer data);

void _games_card_theme_emit_changed (GamesCardTheme *theme);

#ifdef HAVE_RSVG

/* GamesCardThemePreimage (abstract) */

#define GAMES_TYPE_CARD_THEME_PREIMAGE            (games_card_theme_preimage_get_type ())
#define GAMES_CARD_THEME_PREIMAGE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME_PREIMAGE, GamesCardThemePreimage))
#define GAMES_CARD_THEME_PREIMAGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME_PREIMAGE, GamesCardThemePreimageClass))
#define GAMES_IS_CARD_THEME_PREIMAGE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME_PREIMAGE))
#define GAMES_IS_CARD_THEME_PREIMAGE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME_PREIMAGE))
#define GAMES_CARD_THEME_PREIMAGE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME_PREIMAGE, GamesCardThemePreimageClass))

typedef struct _GamesCardThemePreimageClass GamesCardThemePreimageClass;
typedef struct _GamesCardThemePreimage      GamesCardThemePreimage;

struct _GamesCardThemePreimageClass {
  GamesCardThemeClass parent_class;

  gboolean needs_scalable_cards;

  void (* clear_sized_theme_data) (GamesCardThemePreimage *card_theme);
};

struct _GamesCardThemePreimage {
  GamesCardTheme parent_instance;

  char *theme_dir;
  char *theme_name;

  GamesPreimage *cards_preimage;
  GamesPreimage *slot_preimage;
  GdkPixbuf *source;
  CardSize subsize;

  CardSize slot_size;
  CardSize card_size;

  guint size_available : 1;

  cairo_font_options_t *font_options;
};

GType games_card_theme_preimage_get_type (void);

void _games_card_theme_preimage_clear_sized_theme_data (GamesCardThemePreimage *theme);

/* GamesCardThemeSVG */

#ifdef ENABLE_CARD_THEME_FORMAT_SVG

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

#endif /* ENABLE_CARD_THEME_FORMAT_SVG */

/* GamesCardThemeKDE */

#ifdef ENABLE_CARD_THEME_FORMAT_KDE

#define GAMES_TYPE_CARD_THEME_KDE            (games_card_theme_kde_get_type ())
#define GAMES_CARD_THEME_KDE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME_KDE, GamesCardThemeKDE))
#define GAMES_CARD_THEME_KDE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME_KDE, GamesCardThemeKDEClass))
#define GAMES_IS_CARD_THEME_KDE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME_KDE))
#define GAMES_IS_CARD_THEME_KDE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME_KDE))
#define GAMES_CARD_THEME_KDE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME_KDE, GamesCardThemeKDEClass))

typedef struct _GamesCardThemeKDEClass GamesCardThemeKDEClass;
typedef struct _GamesCardThemeKDE      GamesCardThemeKDE;

GType games_card_theme_kde_get_type (void);

GamesCardTheme* games_card_theme_kde_new (void);

#endif /* ENABLE_CARD_THEME_FORMAT_KDE */

/* */

#endif /* HAVE_RSVG */

/* GamesCardThemeSliced */

#ifdef ENABLE_CARD_THEME_FORMAT_SLICED

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

#endif /* ENABLE_CARD_THEME_FORMAT_SLICED */

/* GamesCardThemeFixed */

#ifdef ENABLE_CARD_THEME_FORMAT_FIXED

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

#endif /* ENABLE_CARD_THEME_FORMAT_FIXED */
/* GamesCardThemePysol */

#ifdef ENABLE_CARD_THEME_FORMAT_PYSOL

#define GAMES_TYPE_CARD_THEME_PYSOL            (games_card_theme_pysol_get_type ())
#define GAMES_CARD_THEME_PYSOL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_THEME_PYSOL, GamesCardThemePysol))
#define GAMES_CARD_THEME_PYSOL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_THEME_PYSOL, GamesCardThemePysolClass))
#define GAMES_IS_CARD_THEME_PYSOL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_THEME_PYSOL))
#define GAMES_IS_CARD_THEME_PYSOL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_THEME_PYSOL))
#define GAMES_CARD_THEME_PYSOL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_THEME_PYSOL, GamesCardThemePysolClass))

typedef struct _GamesCardThemePysolClass GamesCardThemePysolClass;
typedef struct _GamesCardThemePysol      GamesCardThemePysol;

GType games_card_theme_pysol_get_type (void);

GamesCardTheme* games_card_theme_pysol_new (void);

#endif /* ENABLE_CARD_THEME_FORMAT_PYSOL */
