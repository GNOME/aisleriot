/*
 * Copyright © 2009, 2010 Christian Persch <chpe@gnome.org>
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
static const GdkColor default_baize_color = { 0, 0 /* red */, 0x5000 /* green */, 0x1000 /* blue */ };

/* The proportion of a slot dedicated to the card (horiz or vert). */
#ifdef HAVE_HILDON
#define DEFAULT_CARD_OVERHANG (0.5)
#else
#define DEFAULT_CARD_OVERHANG (0.0)
#endif

#define DEFAULT_CARD_SLOT_RATIO (0.95)

#define MIN_CARD_STEP (0.0)
#define MAX_CARD_STEP (1.0) /* FIXMEchpe: allow values > 1.0 here? */
#define DEFAULT_CARD_STEP (0.2)

#ifdef HAVE_HILDON
#define DEFAULT_PIXBUF_DRAWING (FALSE)
#else
#define DEFAULT_PIXBUF_DRAWING (TRUE)
#endif

#define DEFAULT_SHOW_TOOLTIPS (FALSE)
#define DEFAULT_SHOW_STATUS_MESSAGES (FALSE)

struct _ArStylePrivate
{
  ArCardTheme* card_theme;

  GdkColor selection_color;
  GdkColor baize_color;

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
  guint enable_tooltips         : 1;
  guint enable_status_messages  : 1;
  guint touchscreen_mode        : 1;

  guint rtl                     : 1;
  guint interior_focus          : 1;

  guint click_to_move           : 1;

  guint keynav_enabled          : 1;
  guint show_focus              : 1;
  guint show_highlight          : 1;
  guint show_seleccion          : 1;

  guint pixbuf_drawing          : 1;
};

G_END_DECLS

#endif /* __AR_STYLE_PRIVATE_H__ */
