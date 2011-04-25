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

#include "ar-card.h"

#ifdef HAVE_RSVG
#include "ar-svg.h"
#endif

#define FLOAT_TO_INT_CEIL(f) ((int) (f + 0.5f))

/* ArCardThemeInfo */

struct _ArCardThemeInfo {
  int ref_count;
  GType type;
  char *path;
  char *filename;
  char *display_name;
  char *pref_name;
  guint scalable : 1;

  gpointer data;
  GDestroyNotify destroy_notify;
};

ArCardThemeInfo *_ar_card_theme_info_new (GType type,
                                                const char *path,
                                                const char *filename,
                                                char *display_name /* adopts */,
                                                char *pref_name /* adopts */,
                                                gboolean scalable,
                                                gpointer data,
                                                GDestroyNotify destroy_notify);

guint _ar_card_theme_info_hash  (const ArCardThemeInfo *a);

int _ar_card_theme_info_collate (const ArCardThemeInfo *a,
                                    const ArCardThemeInfo *b);

/* ArCardTheme */

/* Return TRUE to continue, FALSE to abort */
typedef gboolean (* ArCardThemeForeachFunc) (ArCardThemeClass *klass,
                                                const char *path,
                                                gpointer data);

struct _ArCardThemeClass {
  GObjectClass parent_class;

  /* class vfuncs */
  ArCardThemeInfo * (* get_theme_info)     (ArCardThemeClass *klass,
                                               const char *dir,
                                               const char *filename);
  gboolean             (* foreach_theme_dir)  (ArCardThemeClass *klass,
                                               ArCardThemeForeachFunc,
                                               gpointer data);

  /* vfuncs */
  gboolean    (* load)              (ArCardTheme *theme,
                                     GError **error);
  gboolean    (* set_card_size)     (ArCardTheme *theme,
                                     int width,
                                     int height,
                                     double proportion);
  void        (* get_card_size)     (ArCardTheme *theme,
                                     CardSize *size);
  double      (* get_card_aspect)   (ArCardTheme *theme);
  GdkPixbuf*  (* get_card_pixbuf)   (ArCardTheme *theme,
                                     int card_id);

  void        (* paint_card)        (ArCardTheme *theme,
                                     cairo_t *cr,
                                     int card_id);
  void        (* set_font_options)  (ArCardTheme *theme,
                                     const cairo_font_options_t *font_options);
};

struct _ArCardTheme {
  GObject parent;

  ArCardThemeClass *klass;

  ArCardThemeInfo *theme_info;
};

void _ar_card_theme_class_get_theme_infos (ArCardThemeClass *klass,
                                              GList **list);
ArCardThemeInfo *_ar_card_theme_class_get_theme_info (ArCardThemeClass *klass,
                                                            const char *dir,
                                                            const char *filename);

gboolean _ar_card_theme_class_foreach_theme_dir (ArCardThemeClass *klass,
                                                    ArCardThemeForeachFunc callback,
                                                    gpointer data);
gboolean _ar_card_theme_class_foreach_env (ArCardThemeClass *klass,
                                              const char *env,
                                              ArCardThemeForeachFunc callback,
                                              gpointer data);

void _ar_card_theme_emit_changed (ArCardTheme *theme);

#ifdef HAVE_RSVG

/* ArCardThemePreimage (abstract) */

#define AR_TYPE_CARD_THEME_PREIMAGE            (ar_card_theme_preimage_get_type ())
#define AR_CARD_THEME_PREIMAGE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_THEME_PREIMAGE, ArCardThemePreimage))
#define AR_CARD_THEME_PREIMAGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_THEME_PREIMAGE, ArCardThemePreimageClass))
#define AR_IS_CARD_THEME_PREIMAGE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_THEME_PREIMAGE))
#define AR_IS_CARD_THEME_PREIMAGE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_THEME_PREIMAGE))
#define AR_CARD_THEME_PREIMAGE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_THEME_PREIMAGE, ArCardThemePreimageClass))

typedef struct _ArCardThemePreimageClass ArCardThemePreimageClass;
typedef struct _ArCardThemePreimage      ArCardThemePreimage;

struct _ArCardThemePreimageClass {
  ArCardThemeClass parent_class;

  void (* clear_sized_theme_data) (ArCardThemePreimage *card_theme);
};

struct _ArCardThemePreimage {
  ArCardTheme parent_instance;

  char *theme_dir;
  char *theme_name;

  ArSvg *cards_svg;
  ArSvg *slot_preimage;
  GdkPixbuf *source;
  CardSize subsize;

  CardSize slot_size;
  CardSize card_size;

  guint size_available : 1;

  cairo_font_options_t *font_options;
};

GType ar_card_theme_preimage_get_type (void);

void _ar_card_theme_preimage_clear_sized_theme_data (ArCardThemePreimage *theme);

/* ArCardThemeSVG */

#ifdef ENABLE_CARD_THEME_FORMAT_SVG

#define AR_TYPE_CARD_THEME_SVG            (ar_card_theme_svg_get_type ())
#define AR_CARD_THEME_SVG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_THEME_SVG, ArCardThemeSVG))
#define AR_CARD_THEME_SVG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_THEME_SVG, ArCardThemeSVGClass))
#define AR_IS_CARD_THEME_SVG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_THEME_SVG))
#define AR_IS_CARD_THEME_SVG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_THEME_SVG))
#define AR_CARD_THEME_SVG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_THEME_SVG, ArCardThemeSVGClass))

typedef struct _ArCardThemeSVGClass ArCardThemeSVGClass;
typedef struct _ArCardThemeSVG      ArCardThemeSVG;

GType ar_card_theme_svg_get_type (void);

ArCardTheme* ar_card_theme_svg_new (void);

#endif /* ENABLE_CARD_THEME_FORMAT_SVG */

/* ArCardThemeKDE */

#ifdef ENABLE_CARD_THEME_FORMAT_KDE

#define AR_TYPE_CARD_THEME_KDE            (ar_card_theme_kde_get_type ())
#define AR_CARD_THEME_KDE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_THEME_KDE, ArCardThemeKDE))
#define AR_CARD_THEME_KDE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_THEME_KDE, ArCardThemeKDEClass))
#define AR_IS_CARD_THEME_KDE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_THEME_KDE))
#define AR_IS_CARD_THEME_KDE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_THEME_KDE))
#define AR_CARD_THEME_KDE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_THEME_KDE, ArCardThemeKDEClass))

typedef struct _ArCardThemeKDEClass ArCardThemeKDEClass;
typedef struct _ArCardThemeKDE      ArCardThemeKDE;

GType ar_card_theme_kde_get_type (void);

ArCardTheme* ar_card_theme_kde_new (void);

#endif /* ENABLE_CARD_THEME_FORMAT_KDE */

/* */

#endif /* HAVE_RSVG */

/* ArCardThemeFixed */

#ifdef ENABLE_CARD_THEME_FORMAT_FIXED

#define AR_TYPE_CARD_THEME_FIXED            (ar_card_theme_fixed_get_type ())
#define AR_CARD_THEME_FIXED(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_THEME_FIXED, ArCardThemeFixed))
#define AR_CARD_THEME_FIXED_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_THEME_FIXED, ArCardThemeFixedClass))
#define AR_IS_CARD_THEME_FIXED(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_THEME_FIXED))
#define AR_IS_CARD_THEME_FIXED_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_THEME_FIXED))
#define AR_CARD_THEME_FIXED_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_THEME_FIXED, ArCardThemeFixedClass))

typedef struct _ArCardThemeFixedClass ArCardThemeFixedClass;
typedef struct _ArCardThemeFixed      ArCardThemeFixed;

GType ar_card_theme_fixed_get_type (void);

ArCardTheme* ar_card_theme_fixed_new (void);

#endif /* ENABLE_CARD_THEME_FORMAT_FIXED */
/* ArCardThemePysol */

#ifdef ENABLE_CARD_THEME_FORMAT_PYSOL

#define AR_TYPE_CARD_THEME_PYSOL            (ar_card_theme_pysol_get_type ())
#define AR_CARD_THEME_PYSOL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_THEME_PYSOL, ArCardThemePysol))
#define AR_CARD_THEME_PYSOL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_THEME_PYSOL, ArCardThemePysolClass))
#define AR_IS_CARD_THEME_PYSOL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_THEME_PYSOL))
#define AR_IS_CARD_THEME_PYSOL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_THEME_PYSOL))
#define AR_CARD_THEME_PYSOL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_THEME_PYSOL, ArCardThemePysolClass))

typedef struct _ArCardThemePysolClass ArCardThemePysolClass;
typedef struct _ArCardThemePysol      ArCardThemePysol;

GType ar_card_theme_pysol_get_type (void);

ArCardTheme* ar_card_theme_pysol_new (void);

#endif /* ENABLE_CARD_THEME_FORMAT_PYSOL */
