/*
 * Copyright © 2009 Christian Persch <chpe@gnome.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __AR_STYLE_PRIVATE_H__
#define __AR_STYLE_PRIVATE_H__

G_BEGIN_DECLS

static const GdkColor default_selection_color = { 0, 0 /* red */, 0 /* green */, 0xaa00 /* blue */ };

/* The proportion of a slot dedicated to the card (horiz or vert). */
#ifndef DEFAULT_CARD_SLOT_RATIO
#ifdef HAVE_HILDON
#define DEFAULT_CARD_SLOT_RATIO (0.9)
#else
#define DEFAULT_CARD_SLOT_RATIO (0.8)
#endif
#endif /* !DEFAULT_CARD_SLOT_RATIO */

#define DEFAULT_CARD_OVERHANG (0.0)

#define MIN_CARD_STEP (0.0)
#define MAX_CARD_STEP (1.0) /* FIXMEchpe: allow values > 1.0 here? */
#define DEFAULT_CARD_STEP (0.2)

#ifdef HAVE_HILDON
#define DEFAULT_PIXBUF_DRAWING (FALSE)
#else
#define DEFAULT_PIXBUF_DRAWING (TRUE)
#endif

struct _ArStylePrivate
{
  GamesCardTheme* card_theme;

#ifdef HAVE_CLUTTER
  ClutterColor selection_color;
#else
  GdkColor selection_color;
#endif

  double card_slot_ratio;
  double card_overhang;
  double card_step;

  int dnd_drag_threshold;
  int double_click_time;
  int focus_line_width;
  int focus_padding;

  guint enable_animations_gtk   : 1;
  guint enable_animations       : 1;
  guint enable_sound_gtk        : 1;
  guint enable_sound            : 1;
  guint touchscreen_mode        : 1;

  guint rtl                     : 1;
  guint interior_focus          : 1;

  guint click_to_move           : 1;

  guint keynav_enabled          : 1;
  guint show_focus              : 1;
  guint show_highlight          : 1;
  guint show_seleccion          : 1;

#ifndef HAVE_CLUTTER
  guint pixbuf_drawing          : 1;
#endif
};

#ifdef HAVE_CLUTTER
void _ar_clutter_color_from_gdk_color (ClutterColor *clutter_color,
                                       const GdkColor *gdk_color);
#endif

G_END_DECLS

#endif /* __AR_STYLE_PRIVATE_H__ */
